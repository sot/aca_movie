#!/usr/bin/env /proj/sot/ska/bin/perlska

######################################################################################
# Name: aca_movie.pl
#
# Aca_movie.pl displays a movie of all ACA Level-0 or Level-1 image data files
# within a specified directory (or the current directory if no directory is
# given).
#
# Tom Aldcroft Jan-2004
######################################################################################

use warnings;
use PDL;
use PDL::NiceSlice;
use PDL::ImageND;
use Astro::FITS::CFITSIO::Simple qw(rdfits);
use Time::HiRes qw(sleep);
use Getopt::Long;
use Carp;
use Data::Dumper;
use Tk;
use Tk::Table;
use Ska::Convert qw(time2date date2time);
use POSIX ();
use Quat;
use File::Temp qw(tempdir);
use Ska::DatabaseUtil qw(sql_connect sql_fetchall_array_of_hashref);
use List::MoreUtils qw(first_index last_index);
use File::Basename qw(basename);

our @slot;
our $MAX_DRC = 14;		# Size in pixels of window containing image data
our $MIN_LOW = 1.0;		# Minimum pixel value
our $DT      = 2.05;		# default time step in movie
our $BIG_TIME = 1e14;		# Large time
our $ZOOM     = 8;
our $MAX_EVENT_LOG_LINES = 500;
our $MAX_CONSEQ_EVENT = 10;
my $MICA_ASP1 = '/data/aca/archive/asp1';

# Define L0 and L1 data fields that get displayed, and the corresponding formats
@ACA_L0_MSIDS = qw(slot QUALITY MJF MNF INTEG GLBSTAT COMMCNT COMMPROG IMGFID1 IMGNUM1 IMGFUNC1 IMGSTAT IMGROW0       
		   IMGCOL0 IMGSCALE BGDAVG IMGFID2 IMGNUM2 IMGFUNC2 BGDRMS TEMPCCD TEMPHOUS TEMPPRIM TEMPSEC       
		   BGDSTAT IMGFID3 IMGNUM3 IMGFUNC3 IMGFID4 IMGNUM4 IMGFUNC4 );
@ACA_L0_FORMAT= qw( %d     %d     %d  %d  %.2f   %d        %d      %d      %d       %d      %d       %d    %d
		     %d     %d        %d     %d       %d       %d    %d       %.1f   %.1f     %.1f     %.1f
		     %d       %d      %d      %d      %d       %d    %d);
@PCAD_L1_MSIDS = qw(slot obc_mag_i obc_ang_y obc_ang_z img_row0 img_col0 bkg_rms bkg_avg cmd_count
		    cmd_progress img_func img_stat global_status bkg_outliers);
@PCAD_L1_FORMAT = qw(%d    %.2f      %.1f       %.1f      %d        %d    %.1f    %.1f    %d
		      %d               %d    %d        %d              %d);

our %asol;  # global info about aspect solution, if needed

# MSIDs which are checked and reported in the event log if != 0
%limit_msids = map {$_ => undef}
  qw(quality glbstat commcnt commprog imgstat cmd_count cmd_progress img_stat global status);

# Set up parameter defaults and get command line options
our %opt = ( slot => "0 1 2 3 4 5 6 7",
             obsid => undef,
	     raw   => 0,
	     loud => 0,
	     tstart => 0,
	     tstop  => $BIG_TIME,
	     canvas_size => $MAX_DRC,
	     zoom => $ZOOM,
	     dt     => $DT,
	     log  => 1,
             dark_cal => 1,
         level1 => 0,
         obsdir => undef,
	   );

GetOptions(\%opt,
	   "slot=s",
           "obsid=i",
	   "raw!",
	   "tstart=s",
	   "tstop=s",
	   "canvas_size=i",
	   "zoom=i",
	   "loud!",
	   "log!",
	   'help!',
	   'overlay!',
	   'asol=s',
       'dark_cal!',
       'level1!',
       'obsdir=s',
	   );

$opt{tstart} = date2time($opt{tstart}) if $opt{tstart} =~ /:/;
# print "opt{tstart} = $opt{tstart}\n"; die;
$opt{tstop} = date2time($opt{tstop}) if $opt{tstop} =~ /:/;
$MAX_DRC = $opt{canvas_size};
$ZOOM = $opt{zoom};

# Define the image for a slot with no current data
@IDLE_IMG = ([]);
for $i (0 .. $MAX_DRC-1) {
    for $j (0 .. $MAX_DRC-1) {
	$IDLE_IMG[$i][$j] = ($i == $j) ? '#000000' : '#ffffff';
    }
}

usage(1) if $opt{help};

$Slot::loud = $opt{loud};
$loud = $opt{loud};

my $obs_dir;
if ($opt{obsdir}){
    $obs_dir = $opt{obsdir};
}
if ($opt{obsid}){
  if ($opt{level1}){
      my $obs_str = sprintf("%05d", $opt{obsid});
      my $obs_top_dir = substr($obs_str, 0, 2);
      my @obs_dirs = sort(glob("${MICA_ASP1}/${obs_top_dir}/${obs_str}_*"));
      if (scalar(@obs_dirs)){
          $obs_dir = $obs_dirs[-1];
          print "using $obs_dir for data_dir\n" if $opt{loud}; 
      }
      else{
          croak("-obsid specified but directory not found in ${MICA_ASP1}/${obs_top_dir}");
      }
  }
  else{
      my $dbh = sql_connect('sybase-aca-aca_read');
      my @obsinfo = sql_fetchall_array_of_hashref($dbh,
                                                  "select * from observations where obsid = $opt{obsid}");
      if (not scalar(@obsinfo)){
          croak("-obsid specified but not found in observations_all sybase table");
      }
      if (scalar(@obsinfo) > 1){
          print "Multi-obi or multiple entries in obs table; using first entry\n";
      }
      my $obi = $obsinfo[0];
      # 5 minutes before kalman to get acquisition sequence
      my $start = time2date(date2time($obi->{'kalman_datestart'}) - 300);
      my $stop = $obi->{'kalman_datestop'};
      my @data_files = get_l0_data($start, $stop);
      $obs_dir = tempdir(CLEANUP => 0);
      for my $file (@data_files){
          symlink $file, $obs_dir . "/" . basename($file);
      }
      $opt{tstart} = date2time($start);
      $opt{tstop} = date2time($stop);
      print "Using temp directory for obsid file list.  Repeat use with:\n";
      print "\t aca_movie.pl -obsdir $obs_dir -tstart $opt{tstart} -tstop $opt{tstop}\n";
  }
}


# Get reasonable list of aca0 files from mica archive
sub get_l0_data{
   my ($start, $stop) = @_;
   my @data_dirs = get_l0_dirs($start, $stop);
   my @data_files;
   for my $slot (0 .. 7){
      my @slot_data_files;
      for my $dir (@data_dirs){
         push @slot_data_files, sort(glob("$dir/aca*_${slot}_*"));
      }
      my @slot_file_times;
      for my $file (@slot_data_files){
          my ($file_tstart) = (basename($file) =~ /\A[^\d]+(\d+)/);
          push @slot_file_times, $file_tstart;
      }
      my $first_file_idx = last_index { $_ < date2time($start)} @slot_file_times;
      my $last_file_idx = first_index { $_ >= date2time($stop)} @slot_file_times;
      if (($first_file_idx > 0) and ($last_file_idx > 0)){
          push @data_files, @slot_data_files[$first_file_idx .. $last_file_idx];
      }
   }
   return @data_files;
}

# Get reasonable list of aca0 data directories (stored in YEAR/DOY directories)
# from mica archive
sub get_l0_dirs{
   my ($start, $stop) = @_;
   my $mica_aca0 = '/data/aca/archive/aca0';
   my @data_dirs;
   my ($start_year, $start_day);
   if ($start =~ /(\d{4}):(\d{3}):\d{2}:\d{2}:\d{2}\.\d{3}/){
      $start_year = $1;
      $start_day = $2;
   }
   push @data_dirs, "$mica_aca0/$start_year/$start_day";
   my ($stop_year, $stop_day);
   if ($stop =~ /(\d{4}):(\d{3}):\d{2}:\d{2}:\d{2}\.\d{3}/){
      $stop_year = $1;
      $stop_day = $2;
   }
   my @all_day_glob;
   # Get all of the day dirs from the range of the start year to the stop year
   # Also get start_year - 1 if available to help with edge cases (data starts
   # just before the beginning of the year).
   for my $year ($start_year - 1 .. $stop_year){
       if (-e "$mica_aca0/$year"){
           push @all_day_glob, sort(glob("$mica_aca0/$year/*"));
       }
   }
   my $day_start = first_index { $_ eq "$mica_aca0/$start_year/$start_day" } @all_day_glob;
   my $day_stop = first_index { $_ eq "$mica_aca0/$stop_year/$stop_day" } @all_day_glob;
   # Get the range of days, plus one extra day at the start to help with day start
   # edge cases
   my @day_glob = @all_day_glob[$day_start - 1 .. $day_stop];
   return @day_glob;
}


if (defined $obs_dir){
  ($data_dir = $obs_dir) and (chdir $data_dir or croak "Could not find directory $data_dir\n");
}
else{
  # Change into specified data directory
  ($data_dir = shift @ARGV) and (chdir $data_dir or croak "Could not find directory $data_dir\n");
}

# For each specified slot, create a new slot object.  This reads in the list
# of files corresponding to each slot and sets the global start and stop time
# of all files for that slot

$opt{slot} =~ s/,/ /g;
@slot = split ' ',$opt{slot};
$tstart = $BIG_TIME;
$tstop  = 0;

foreach $s (@slot) {
    next unless $slts[$s] = Slot->new(slot => $s,
				      use_raw => $opt{raw},
				      canvas_size => $opt{canvas_size},
				     );
    $file_level = $slts[$s]->{file_level}; # Indicates ACA L0 (0) or PCAD L1 ACADATA (1)
    $tstart = $slts[$s]->{tstart} if $slts[$s]->{tstart} < $tstart;
    $tstop = $slts[$s]->{tstop} if $slts[$s]->{tstop} > $tstop;
}
die "No image data files!\n" if ($tstop == 0);

# Now show the movie of images corresponding to the entire range of
# selected files, or a subset if specified on command line

$tstart = $opt{tstart} if $opt{tstart} > $tstart;
$tstop  = $opt{tstop}  if $opt{tstop}  < $tstop;
$| = 1;
$time   = ($opt{dt} > 0) ? $tstart : $tstop;
$time_direction = 1;

my $dc;
if ($opt{dark_cal}){
  if ($opt{asol}){
    print "Dark Cal background image not implemented in -asol mode\n";
  }
  my $tstart_date = time2date($tstart);
  my $tstr;
  if ($tstart_date =~ '(\d{4}):(\d{3}):.*'){
    $tstr = "$1$2";
  }
  my @dark_cals = reverse(sort(glob('/proj/sot/ska/data/aca_dark_cal/[12][0-9][0-9][0-9][0-9][0-9][0-9]/')));
  for my $poss_dc (@dark_cals){
    if ($poss_dc =~ '/proj/sot/ska/data/aca_dark_cal/(\d{7})'){
      my $date_str = $1;
      # if the date of the dark cal is before tstart (represented as just YYYYDOY in tstr),
      # read that dark cal and leave the loop.
      if ($date_str < $tstr){
        print "Reading dark cal from $poss_dc\n" if $opt{loud};
        $dc = rdfits("${poss_dc}/imd.fits");
        $dc = dark_cal_bgd_subtract($dc);
        last;
      }
    }
  }
  if (not defined $dc){
    croak("Problem loading dark cal data");
  }
}

# Create the main display
make_gui();

# Refine %limit_msids so that values contain the indices (relative
# to @info_msid of valid msids for this dataset
set_limit_indices();

$show_image_id = $top->after(0, \&show_image_frame);
MainLoop();

print "\nDone\n";
##****************************************************************************
sub dark_cal_bgd_subtract {
# subtract off the value of the peak of the dark cal histogram
#
##****************************************************************************
    my $all_dc = shift;

    ($xvals, my $dc_hist) = hist($all_dc, (0,30,1));
    my $peak =  $xvals(which($dc_hist == $dc_hist->(1:-2)->max()))->at(0);
    my $sdc = $all_dc - $peak;
    my $DC_LOW = 1;
    $sdc = $sdc + $DC_LOW;
    # For log scaling, make sure lower bound is positive
    if ($opt{log}){
      $sdc = log10($sdc * ($sdc > $DC_LOW) + $DC_LOW * ($sdc <= $DC_LOW))
    }
    else{
      $sdc = $sdc * ($sdc > $DC_LOW) + $DC_LOW * ($sdc <= $DC_LOW);
    }

    return $sdc;
  }

##***************************************************************************
sub set_limit_indices {
# Refine %limit_msids so that values contain the indices (relative
# to @info_msid of valid msids for this dataset.  Delete any used MSIDs
##***************************************************************************
    local $_;
    foreach (0 .. $#info_msid) {
	my $msid = $info_msid[$_];
	$limit_msids{lc $msid} = $_ if exists $limit_msids{lc $msid};
    }
    map { delete $limit_msids{$_} unless defined $limit_msids{$_} } keys %limit_msids;
}

##***************************************************************************
sub make_gui {
## Graphics initialization
##***************************************************************************
  $top = MainWindow->new();
  
  # Pixel images of 8 slots, at top
  $slot_img = $top->Photo( 'slot_img' , -palette => 256/256/256);
  $all_img  = $top->Photo( 'all_img' , -palette => 256/256/256);
  $all_img->put (("#ffffff"), -to => (0, 0, $MAX_DRC * 8 * $ZOOM + 9, $MAX_DRC * $ZOOM));
  $top->Label('-image'=> $all_img  )->pack;
  
  # Frame with all the other information and buttons

  $info_and_buttons = $top->Frame();
  $info_and_buttons->pack();

  # Times and status bits

  @ts = (['Time',   \$format_time],
	 ['Date',   \$tlm_date],
	);
	 
  $times_and_status = $info_and_buttons->Table(-columns => 2,
					       -rows    => $#ts+1,
					       -scrollbars => 'o',
					       );
  $times_and_status->pack(-expand=> 1, -fill => 'both', -side => 'left');
  for $i (0 .. $#ts) {
    my $l = $times_and_status->Label(-text => $ts[$i][0], -relief => 'groove');
    $times_and_status->put($i, 0, $l);
    $l = $times_and_status->Label(-textvariable => $ts[$i][1], -relief => 'groove');
    $times_and_status->put($i, 1, $l);
  }

  # ACA info table for each of eight slots

  @info_msid   = $file_level ? @PCAD_L1_MSIDS : @ACA_L0_MSIDS;
  @info_format = $file_level ? @PCAD_L1_FORMAT : @ACA_L0_FORMAT;
  
  $info_table  = $info_and_buttons->Table(-columns => 9,
					  -rows => $#info_msid+1,
					  -scrollbars => 'o',
					 );
  $info_table->pack(-expand=> 1, -fill => 'both', -side => 'left');
  
  for $j (0..$#info_msid) {
    for $i (0..8) {
      $info_table_txt[$j][$i] = ($i == 0) ? $info_msid[$j] : '      ';
      my $l = $info_table->Label(-textvariable => \$info_table_txt[$j][$i],
				 -relief => 'groove');
      $info_table->put($j, $i, $l);
    }
  }
  
  # Buttons to select time step

  $dt_radio_buttons = $info_and_buttons->Frame(-relief => 'raised')->pack(-side => 'left', -anchor=>'n');
  $dt_radio_buttons->Label(-text => 'Time step')->pack(-side=>'top');

  our %dt_radio;
  foreach (1.025, 2.05, 4.1, 8.2, 16.4, 32.8, 131.2,1049.6) {
      $dt_radio{$_} = $dt_radio_buttons->Radiobutton( -variable => \$opt{dt},
						    -text     => $_,
						    -value    => $_)->pack(-side=>'top',
									   -anchor=>'w');
  }
  
  # Pause/resume, time direction, and Quit buttons

  our $b_pause = $info_and_buttons->Button(-text => 'Pause',
					   -command => \&pause,
					  )->pack;

  our $b_next = $info_and_buttons->Button(-text => 'Next',
					  -command => \&show_image_frame
					 )->pack;
  
  $radio{forward} = $info_and_buttons->Radiobutton(-text => 'Forward',
						   -variable => \$time_direction,
						   -value    => 1,
						  )->pack;
  $radio{reverse} = $info_and_buttons->Radiobutton(-text => 'Reverse',
						   -variable => \$time_direction,
						   -value    => -1,
						  )->pack;
  
  $update_delay = 0;
  our $delay = $info_and_buttons->Scale(-orient => 'vertical',
					-from   => 0,
					-to     => 2000,
					-tickinterval => 500,
					-label  => 'Delay',
					-variable => \$update_delay
				       )->pack;

  our $b_quit = $info_and_buttons->Button(-text => 'Quit',
				      -command => \&end_aca_movie,
					 )->pack;
  
  # Event log

  $top->Label(-text => 'Event log')->pack();
  $event_log = $top->Scrolled('Text', height => 12, width => 100);
  $event_log->pack(-side => 'top', -anchor => 'w');

}

##
sub pause {
    if ($b_pause->cget('-text') eq "Pause") {
	$b_pause->configure(-text => 'Resume');
	$top->afterCancel($show_image_id);
    } else {
	$b_pause->configure(-text => 'Pause');
	show_image_frame();
    }
    $top->update();
}

##***************************************************************************
sub show_image_frame {
# 
# Main processing loop, which gets images and telemetry data at a specific time,
# and updates the event log as necessary
#
##***************************************************************************
    $tlm_date = time2date($time);
    $format_time = sprintf "%.2f", $time;

    get_image_frame();

    # Come on back y'all!  (Unless the pause button was pressed)
    $show_image_id = $top->after ($update_delay, \&show_image_frame)
      unless ($b_pause->cget('-text') eq "Resume");  
}

##***************************************************************************
sub get_aspect_offset {
##***************************************************************************
    my $asol_file = shift;
    my $time = shift;
    if (not defined $asol{file}) {
	# First time through
	$asol{file} = $asol_file;
	$asol{data} = { CFITSIO::Simple::fits_read_bintbl($asol{file}, qw(time ra dec roll)) };
	$asol{hdr}  = CFITSIO::Simple::fits_read_hdr("$asol{file}\[1\]");
	$asol{q0} = Quat->new($asol{hdr}->{RA_NOM}, $asol{hdr}->{DEC_NOM}, $asol{hdr}->{ROLL_NOM});
	$asol{last_i} = 0;
	$asol{n_row} = $asol{data}->{time}->nelem();
    }
    my $i = $asol{last_i};
    while ($i < $asol{n_row} && $asol{data}->{time}->at($i) < $time) {
	$i++;
    }
    if ($i != $asol{last_i}) {
	$asol{q} = Quat->new($asol{data}->{ra}->at($i),
			     $asol{data}->{dec}->at($i),
			     $asol{data}->{roll}->at($i));
	$asol{dq} = $asol{q}->divide($asol{q0});
	$asol{dy} = $asol{dq}->{ra0} * 3600;   # For small angles this is correct 
	$asol{dz} = $asol{dq}->{dec} * 3600;   # (normally use radec2yagzag)
	$asol{last_i} = $i;
    }
    return ($asol{dy}, $asol{dz});
}


sub get_image_frame {
# Go through all slots and assemble a complete 'frame' of data (images
# and telemetry variables)
##***************************************************************************
    my $canvas_sz = $opt{canvas_size};
    my ($dy, $dz) = get_aspect_offset($opt{asol}, $time) if $opt{asol};

    for $s (@slot) {
	# Clear all the information table information for this slot and set all
	# limit-checked msids to have the default background color
        for my $im (0 .. $#info_msid){
	  $info_table_txt[$im][$s+1] = '      ';
        }
	foreach (values %limit_msids) {
	    $info_table->get($_, $s+1)->configure(-background=>'#d5d5d5');
	}

	# Try to get an image record at this time for this slot.  If not, just
	# set the image to a predefined 'idle' image
	unless ($slt = $slts[$s] and $img = $slt->get_image($time)) {
	    $slot_img->put(\@IDLE_IMG);
	    $all_img->copy ($slot_img, -zoom => $ZOOM, -to => ($s*$canvas_sz*$ZOOM+$s+1, 0));
	    next;
	}

	print "Processing slot $s\n" if $loud;

	$slt->{canvas} = zeroes($opt{canvas_size}, $opt{canvas_size})
	  unless $opt{overlay};

        my $cmask;
	if (defined $dy and defined $dz) {
	    $slt->put_img_to_canvas_sky($img, $dy, $dz);
	} else {
	    $cmask = $slt->put_img_to_canvas_ccd($img);
	}	    

	# Set the color array for the Tk image
	@carr = ([]);
	for $i (0 .. $canvas_sz-1) {
	    for $j (0 .. $canvas_sz-1) {
		$color = sprintf ("%02x", $slt->{canvas}->at($canvas_sz-1-$i,$j));
		push @{$carr[$i]}, "#$color$color$color";
	    }
	}

	# Change the background when using a dark cal
        if (defined $dc){
          for $i (0 .. $canvas_sz-1) {
	    for $j (0 .. $canvas_sz-1) {
              my $v =  $slt->{canvas}->at($canvas_sz-1-$i, $j);
              my $live_img = $cmask->at($canvas_sz-1-$i, $j);
              if (not $live_img){
                $color1 = sprintf ("%02x", $slt->{canvas}->at($canvas_sz-1-$i,$j) + 30);
                $color2 = sprintf ("%02x", $slt->{canvas}->at($canvas_sz-1-$i,$j));
                $color3 = sprintf ("%02x", $slt->{canvas}->at($canvas_sz-1-$i,$j));
                $carr[$i]->[$j] = "#$color1$color2$color3";
              }
            }
          }
        }
	# Put the color array into an image for slot, then copy into main window
	$slot_img->put(\@carr);
	$all_img->copy ($slot_img, -zoom => $ZOOM, -to => ($s*$canvas_sz*$ZOOM+$s+1, 0));

	# Set telemetry information table and check limits
	foreach $j (0 .. $#info_msid) {
	    my $msid = lc $info_msid[$j];
	    if (exists $img->{$msid}) {
		$info_table_txt[$j][$s+1] = sprintf $info_format[$j], $img->{$msid}->at(0);

		# check limit if specified
		if (defined $limit_msids{$msid}) {
		    if ((my $value = $img->{$msid}->at(0)) != 0) {
			$info_table->get($j, $s+1)->configure(-background => 'red');
			$n_conseq_events[$j][$s]++;
		    } else {
			$n_conseq_events[$j][$s] = 0;
		    }
		}
	    }
	}
	    
	if ($loud) {
	    print "slot: $s row: $img->{row0}  col:  $img->{col0}\n";
	    print $img->{img};
	    print "slot $s ", $img->{row0}, " ", $img->{col0}, " ", $slt->{sz}, " \n";
	}
    }
    update_event_log();

    # After all slots processed, update the display
    $top->update();

    # Go to next time step, but don't go outside allowed tstart/tstop
    $time += $opt{dt} * $time_direction;
    $time = $tstart if ($time < $tstart);
    $time = $tstop  if ($time > $tstop);
}

##***************************************************************************
sub update_event_log {
##***************************************************************************
#  $event_log->insert('end', "$vcdu: hello \n");
    foreach my $j (0 .. $#info_msid) {
	my $msid = lc $info_msid[$j];
	next unless defined $limit_msids{$msid};
	my $event = 0;
	my $event_string = "$tlm_date :: $msid : ";
	for $s (@slot) {
	    my $val = $info_table_txt[$j][$s+1];
	    $val = '--' unless ($val =~ /\S/);
	    $event_string .= " $val";
	    $event = 1 if ($val ne '--' and $n_conseq_events[$j][$s]
			   and $n_conseq_events[$j][$s] < $MAX_CONSEQ_EVENT);
	}
	$event_log->insert('end', "$event_string\n") if ($event);
    }

    $event_log->see('end');
    if ($event_log->index('end') > $MAX_EVENT_LOG_LINES) {
	$event_log->delete("1.0 linestart", "1.0 lineend + 1 c");
    }
}

##***************************************************************************
sub end_aca_movie {
##***************************************************************************
    print STDERR "ACA_MOVIE% Finished. \n";
    $top->withdraw();
    exit(0);
}


##***************************************************************************
sub usage
##***************************************************************************
{
  my ( $exit ) = @_;

  local $^W = 0;
  require Pod::Text;
  Pod::Text::pod2text( '-75', $0 );
  exit($exit) if ($exit);
}

=pod

=head1 NAME

aca_movie.pl - Play a movie of all ACA Level-0 or Level-1 image data files in a directory

=head1 SYNOPSIS

B<aca_movie.pl>  [I<options>] [<Image_directory>]

=head1 OPTIONS

=over 4

=item B<-help>

Print this help information.

=item B<-obsid <obsid>>

Display data for obsid <obsid>.  Makes a tempdir with a list of links to mica archive l0 files.

=item B<-level1>

In combination with the -obsid option, display mica archive level 1 products instead of l0.

=item B<-obsdir <directory>>

Display data in director <directory>.

=item B<-dark_cal>

Enable/disable use of dark cal image in the window background
(Enabled by default, use -no_dark_cal to disable).

=item B<-slot <slots>>

Play movie for slots <slots>, which should be a space- or comma-separated list
of slots.  Default is for all 8 slots.

=item B<-raw>

Use the raw image (instead of the corrected image) for L1 image data.

=item B<-tstart <time>>

Start movie at <time>, which can be in the CXC time format (seconds since
1998.0) or as a date in the format YYYY:DOY:HH:MM:SS.  DOY is the day of year,
e.g. 2001:198:12:04:22.  By default tstart is set to the first valid time
within the available data files.

=item B<-tstop <time>>

Stop movie at <time>, where the time format is the same as for -tstart.  By
default tstart is set to the last valid time within the available data files.

=item B<-canvas_size <size>>

Make the fixed image window be <size> x <size> pixels.  Default is 14.

=item B<-zoom <scale>>

Zoom the image data by <scale>.  Default is 8.

=item B<-overlay>

Overlay new image data instead of erasing first.

=item B<-asol <asol_file>>

Use the supplied aspect solution file <asol_file> to display the image data
on a fixed sky grid instead of the default fixed CCD grid.  The <asol_file> must be in the
<Image_directory> containing the image data files.

=item B<-loud>

Enable debugging information including image pixel values.

=item B<-log>

Display image pixel values in log scale (True by default, use -no_log for linear scale)

=back

=head1 DESCRIPTION

B<aca_movie.pl> displays a movie of all ACA Level-0 or Level-1 image data files
within a specified directory (or the current directory if no directory is
given).

Level-0 files are chosen as any files matching aca*<slot>_img0.fits*, while
Level-1 files must match pcad*adat<slot>?.fits*.  Files can be gzipped.  If
both L0 and L1 files are present, only the L0 files will be selected.

The "Time step" radio button list lets you change the sampling, up to a maximum
of about 1 ksec per step.

The "delay" slider on the lower-right lets you slow down the movie (mostly
useful for a fast Linux machine), while the Pause button lets you stop the
action entirely.

Clicking on 'reverse' will make the movie go backwards.  If you have paused,
then 'Next' will go forward one time step.  

The text box on the bottom records any instances in which a status bit or the
command count or progress are non-zero.

When no data are available for a particular slot, the image is completely white
with a black slash from corner to corner.

=head1 AUTHOR

Tom Aldcroft ( taldcroft@cfa.harvard.edu )

=cut




package Slot;

use PDL;
use PDL::NiceSlice;
use CFITSIO::Simple;
use Carp;
use Data::Dumper;
use Ska::ACACoordConvert qw(toAngle);

our $BIG_TIME = 1e14;

##****************************************************************************
sub new {
##****************************************************************************
    my $classname = shift;
    my $self = {};
    bless ($self);
    my @files;

    # Copy all the info from new call
    %{$self} = ( tstart => 0,
		 tstop  => $BIG_TIME,
		 slot   => 0,
		 time   => 0,
		 index  => 0,
		 sz     => 0,
		 file   => {name   => '',
			    tstart => -1,
			    tstop  => -1,
			    dt     => -1,
			    n      => -1},
		 file_list   => [],
		 file_info   => {}, # Hashref of file information/data
		 canvas_size     => 14,
		 @_
	       );

    # Initialize the list of appropriate L0 or L1 ACA image files
    unless (@{$self->{file_list}}) {
	my $l0_glob = "aca*$self->{slot}_img0.fits*";
	my $l1_glob = "pcad*adat$self->{slot}?.fits*";
	$self->{file_level} = (@files = glob($l0_glob)) ? 0 : 1;
	@files = glob($l1_glob) unless @files;
	$self->{file_list} = [@files];
    }

    return unless @files;

    # Set reasonable value of tstart based on first file name timestamp
    # and then read last file for final tstop
    my ($file_tstart) = ($files[0] =~ /\A[^\d]+(\d+)/);
    $self->{tstart} = $file_tstart if $file_tstart > $self->{tstart};
    my $hdr = fits_read_hdr($files[-1]);
    $self->{tstop} = $hdr->{TSTOP};
    $self->{canvas} = zeroes($self->{canvas_size}, $self->{canvas_size});

    return $self;
}

##****************************************************************************
sub get_file {
##****************************************************************************
    my $self = shift;
    my $time = $self->{time};
    local $_;
    my ($name, $info);
    
    # First delete any info entries that are no longer needed
    #    (not yet implemented)

    # Make sure we start at the beginning of any new file
    $self->{index} = 0;

    # Then look through any files which may have been read earlier
    while (($name, $info) = each %{$self->{file_info}}) {
	if ($info->{tstart} <= $time && $time < $info->{tstop}) {
	    print "Found a file that was already read in for slot $self->{slot} at $time\n" if $loud;
	    $self->{file} = $info;
	    $self->scale_image_data(); # Scale, calibrate image data (take log and scale into range 0..255)
	    return 1;
	}
    }

    # Now go through all input data files in reverse order and search for
    # the one containing the desired time
    foreach $name (reverse @{$self->{file_list}}) {
	next if $self->{file_info}{$name}; # Already read and checked this one
	my ($file_tstart) = ($name =~ /\A[^\d]+(\d+)/);
	croak "Could not get a tstart from file name $name\n" unless $file_tstart;
	next unless ($time > $file_tstart - 5);

	# This file may include desired record.  Read it, keep it available, and
	# if it actually has needed data, then return.
	print "Reading file $name for slot $self->{slot} at time $time\n" if ($loud);
	  
	my %data = fits_read_bintbl($name);
	if (exists $data{img_corr}) { # It's L1 data, make it look like L0
	    $data{img} = $self->{use_raw} ? $data{img_raw} : $data{img_corr};
	    $data{row0} = $data{img_row0};
	    $data{col0} = $data{img_col0};
	} else {
	    $data{img} = $data{imgraw};
	    $data{row0} = $data{imgrow0};
	    $data{col0} = $data{imgcol0};
	}

	# Fix a bug in CXCDS L0 decom which leaves the command count and progress
	# shifted left by two bits.  We don't care about those two bits, so just
	# shift right two bits.  (The trailing 0 in the shiftright function is just
	# a weird calling requirement of the function).
	for (qw(cmd_count cmd_progress commcnt commprog)) {
	    $data{$_}->inplace->shiftright(2,0) if defined $data{$_};
	}

	# Create an info record for this file and store 
	my $hdr = fits_read_hdr($name, 'ACADATA');
#	my $dt = $data{time}->at(1) - $data{time}->at(0); # uff, should use hdr
	my $dt = $hdr->{TIMEDEL};
	my $tstart = $data{time}->at(0) - $dt/2;
	my $tstop = $data{time}->at(-1) + $dt/2;
	my ($sz) = dims $data{img};
	my $info =  { name => $name,
		      data => \%data,
		      tstart => $tstart,
		      tstop  => $tstop,
		      dt     => $dt,
		      n      => $data{time}->nelem,
		      sz     => $sz,
		      scaled => 0,
		    };
	
	$self->{file_info}{$name} = $info;

	# If the desired time is in this file, return success!
	if ($tstart <= $time && $time < $tstop) {
	    $self->{file} = $info;
	    $self->scale_image_data(); 
	    return 1;
	}
    }

    return;
}

##****************************************************************************
sub put_img_to_canvas_ccd {
#
# Set object variables which control brightness scaling and
#  CCD region corresponding to first image
# Apply some simple calibrations and manipulations to table data
#
##****************************************************************************
    my $self = shift;
    my $img = shift;
    
    # Set the corners of the image relative to current extended image window
    my $sz = $self->{file}{sz};

    my $sc0 = $img->{col0} - $self->{c0};
    my $sc1 = $sc0 + $sz - 1;
    my $sr0 = $img->{row0} - $self->{r0};
    my $sr1 = $sr0 + $sz - 1;
	
    # Check if image falls outside extended image window
    if ($sc0 < 0) {
	$self->{c0} = $img->{col0};
    } elsif ($sc1 >= $self->{canvas_size}) {
	$self->{c0} = $img->{col0} + $sz - $self->{canvas_size};
    }
    if ($sr0 < 0) {
	$self->{r0} = $img->{row0};
    } elsif ($sr1 >= $self->{canvas_size}) {
	$self->{r0} = $img->{row0} + $sz - $self->{canvas_size};
    }


    if (defined $dc){
      eval{
           # put the dark cal image up there
          $win_dc = $dc->($self->{c0} - 512:$self->{c0} + $self->{canvas_size} - 513,
                          $self->{r0} - 512:$self->{r0} + $self->{canvas_size} - 513);
          # scale it using the image data as a reference
          my $scaled_dc = scale_dc($win_dc, $self);
          $self->{canvas} .= $scaled_dc;

      };
      if ($@){
          print $@;
      }
    }

    # recalculate since $c0 or $r0 may have changed.  
    $sc0 = $img->{col0} - $self->{c0};
    $sc1 = $sc0 + $sz - 1;
    $sr0 = $img->{row0} - $self->{r0};
    $sr1 = $sr0 + $sz - 1;

    # Copy the individual image into extended image window
    $self->{canvas}->($sc0:$sc1, $sr0:$sr1) .= $img->{img};

    # make a mask to note which values of the canvas are real image data
    my $color_mask = zeros($self->{canvas});
    $color_mask->($sc0:$sc1, $sr0:$sr1) .= 1;
    return $color_mask;
}


##****************************************************************************
sub put_img_to_canvas_sky {
#
# Set object variables which control brightness scaling and
#  CCD region corresponding to first image
# Apply some simple calibrations and manipulations to table data
#
##****************************************************************************
    my $self = shift;
    my $img = shift;
    my $dy = shift;
    my $dz = shift;
    
    # Set the corners of the image relative to current extended image window
    my $sz = $self->{file}{sz};
    my $cansz2 = $self->{canvas_size}/2;

    # Coordinates of lower-left image pixel in ACA yag,zag
    my ($y0, $z0) = toAngle(sclr($img->{row0}), sclr($img->{col0}));
    if (not defined $self->{y0}) {
	($self->{y0}, $self->{z0}) = ($y0, $z0);
    }
    # Add offset of the ACA yag,zag frame based on aspect solution offset
    print "Aspect offset = $dy $dz\n" if $loud;
    $y0 += $dy;  
    $z0 += $dz;  

    my $c_r0 = POSIX::floor( ($z0 - $self->{z0}) / 5.0 + 0.5);
    my $c_c0 = POSIX::floor(-($y0 - $self->{y0}) / 5.0 + 0.5);
    for $img_row (0..$sz-1) {
	for $img_col (0..$sz-1) {
	    #  The 'transposition' of c_c0+row and c_r0+col is correct!
	    $self->{canvas}->($c_c0 + $img_row + $cansz2, $c_r0 + $img_col + $cansz2)
	      .= $img->{img}->at($img_col, $img_row);
	}
    }
}



##****************************************************************************
sub scale_dc {
##****************************************************************************
    my $dc_img = shift;
    my $self = shift;

    # use the percentiles from the image data to scale the dark cal
    # this is just a placeholder.
    my $img_98th = $self->{file}{data}{img_98th};
    my $img_2nd = $self->{file}{data}{img_2nd};
    my $MAX = 225;
    $dc_img = $MAX * ($dc_img - $img_2nd) / ($img_98th - $img_2nd);
    $dc_img = $dc_img * ($dc_img <= $MAX) + $MAX * ($dc_img > $MAX);
    $dc_img = $dc_img * ($dc_img >= 0) + 0 * ($dc_img < 0);
    return byte($dc_img);
  }



##****************************************************************************
sub scale_image_data {
#
# Set object variables which control brightness scaling and
#  CCD region corresponding to first image
# Apply some simple calibrations and manipulations to table data
#
##****************************************************************************
    my $self = shift;
    my $xvals;
    my $MIN_LOW = 1.0;
    my $sz  = $self->{file}{sz};
    local $_;
    $self->{last_sz} = $self->{sz};
    $self->{sz} = $sz;

    # Return if the image data were already scaled
    return if $self->{file}{data}{scaled};

    my $img = $self->{file}{data}{img} + $MIN_LOW + 5.0;

    # For log scaling, make sure lower bound is positive
    $img = log10($img * ($img > $MIN_LOW) + $MIN_LOW * ($img <= $MIN_LOW))
      if $opt{log};

    # Generate histogram of pixel values, add this to any existing histogram, and
    # then make the cumulative histogram
    my @hist_binning = $opt{log} ? (0, 5, 0.005) : (-200, 200000, 20);
    ($xvals, $self->{file}{hist}) = hist($img, @hist_binning);

    $self->{hist} = zeroes($self->{file}{hist}) unless (exists $self->{hist} and $sz == $self->{last_sz});
    $self->{hist} += $self->{file}{hist};
    my $cumsum = cumusumover $self->{hist};

    # Get the index of the 2nd and 98th percentile, then scale to max of 255 and clip any
    # outliers to 0, 255
    my $ok = which ($cumsum > 0.98*$cumsum->at(-1));
    my $img_98th = $xvals($ok)->at(0);
    $ok = which ($cumsum > 0.02*$cumsum->at(-1));
    my $img_2nd = $xvals($ok)->at(0);
    $self->{file}{data}{img_98th} = $img_98th;
    $self->{file}{data}{img_2nd} = $img_2nd;
    print "img_98th for slot $self->{slot} = $img_98th\n" if $loud;
    print "img_2nd for slot  $self->{slot} = $img_2nd\n" if $loud;
    $img = 255.0 * ($img - $img_2nd) / ($img_98th - $img_2nd);
    $img = $img * ($img <= 255) + 255 * ($img > 255);
    $img = $img * ($img >= 0) + 0 * ($img < 0);
    $self->{file}{data}{img} = byte($img);
    
    # Set row,col limits if not already set.  (Normally these variables are
    # manipulated in the plotting itself)
    unless (exists $self->{r0} and exists $self->{c0}) {
	$self->{r0} =  sprintf("%d", $self->{file}{data}{row0}->at(0) + $sz/2 - $MAX_DRC/2);
	$self->{c0} =  sprintf("%d", $self->{file}{data}{col0}->at(0) + $sz/2 - $MAX_DRC/2);
    }

    # Apply some simple calibrations to table data
    # 
    # Convert L0 temperatures from Kelvins to Celsius
    my $data = $self->{file}{data};
    map {$data->{$_} -= 273.15 if exists $data->{$_}} qw(tempccd temphous tempprim tempsec);
    # Convert L1 angles from degrees to arcsec
    map {$data->{$_} *= 3600 if exists $data->{$_}} qw(obc_ang_y obc_ang_z);
    # Create L1 img_func and img_stat from img_func_stat
    if (exists $data->{img_func_stat}) {
	$data->{img_func} = $data->{img_func_stat} >> 6;
	$data->{img_stat} = $data->{img_func_stat} % 64;
    }
    $data->{slot} = ones($data->{time}) * $self->{slot};

    # Mark the dataset as scaled
    $self->{file}{data}{scaled} = 1;
}

##****************************************************************************
sub get_image {
##****************************************************************************
    my $self = shift;
    $self->{time} = shift || croak "Slot::getimage(): Need a time\n";
    my $time = $self->{time};
    local *i;
    local $_;

    print "Trying to get image at time $time\n" if $loud;
    # Get initial file and return () unless something covering the right
    # time range was found
    return unless (($self->{file}{tstart} <= $time && $time < $self->{file}{tstop})
		   or $self->get_file());

    my $data = $self->{file}{data};
    *i = \$self->{index};

    my $delta_t = $time - $data->{time}->at($i);
    my $min_abs_delta_t = abs($delta_t);
    my $d_i = ($delta_t > 0 ? +1 : -1);
    while (1) {
	$i += $d_i;
	if ($i < 0) {
	    $i = 0;
	    last;
	}
	if ($i >= $self->{file}{n}) {
	    $i = $self->{file}{n} - 1;
	    last;
	}

	$delta_t = $time - $data->{time}->at($i);
	if (abs($delta_t) < $min_abs_delta_t) {
	    # Getting close to desired time
	    $min_abs_delta_t = abs($delta_t);
	} else {
	    # Went too far, so last index must have been the best (since
	    # sequence is monotonic)
	    $i -= $d_i;
	    print "i = $i $time ",$data->{time}->at($i),"\n" if $loud;
	    last;
	}
    }
    
    my %img = ();

    while (($name, $data) = each %{$self->{file}{data}}) {
	if (UNIVERSAL::isa($data,'PDL')) {
	    my $slice = ':,' x (scalar($data->dims)-1) . $i;
	    $img{$name} = $data->slice($slice)->reshape(-1);
	} else {
	    $img{$name} = $data->[$i];
	}
    }
    return \%img;
}

1;

# ColNo  Name                 Unit        Type             Range
#    1   TIME                 s            Real8          176246164.8726499975:176267165.0735520124 Time-tag of the data record
#    2   QUALITY                           Int4           -                    Data quality flag; 0 - good, 1 - bad
#    3   MJF                               Int4           0:131071             Major frame ctr value
#    4   MNF                               Int4           -                    Minor frame ctr value
#    5   END_INTEG_TIME       s            Real8          -Inf:+Inf            end integration time
#    6   INTEG                s            Real4          -Inf:+Inf            integration time
#    7   GLBSTAT                           Byte                                global status
#    8   COMMCNT                           Byte                                command count
#    9   COMMPROG                          Byte                                command progress
#   10   IMGFID1                           Byte                                image type
#   11   IMGNUM1                           Byte                                image number (of 8)
#   12   IMGFUNC1                          Byte                                image function
#   13   IMGSTAT                           Byte                                image status
#   14   IMGROW0                           Int2           -511:512             row of lowerleft image pixel
#   15   IMGCOL0                           Int2           -511:512             col of lowerleft image pixel
#   16   IMGSCALE                          UInt2          0:1023               pixel scaling factor
#   17   BGDAVG               DN           UInt2          -                    average background
#   18   IMGFID2                           Byte                                image type
#   19   IMGNUM2                           Byte                                image number (of 8)
#   20   IMGFUNC2                          Byte                                image function
#   21   BGDRMS               DN           UInt2          -                    background RMS
#   22   TEMPCCD              K            Real4          -Inf:+Inf            temp 1 - CCD
#   23   TEMPHOUS             K            Real4          -Inf:+Inf            temp 2 - AC housing
#   24   TEMPPRIM             K            Real4          -Inf:+Inf            temp 3 - lens cell
#   25   TEMPSEC              K            Real4          -Inf:+Inf            temp 4 - secondary mirror
#   26   BGDSTAT                           Byte                                bgd pixel status
#   27   IMGFID3                           Byte                                image type
#   28   IMGNUM3                           Byte                                image number (of 8)
#   29   IMGFUNC3                          Byte                                image function
#   30   IMGFID4                           Byte                                image type
#   31   IMGNUM4                           Byte                                image number (of 8)
#   32   IMGFUNC4                          Byte                                image function
#  
#    1   time                 sec          Real8          176267231.8755553961:176267330.2755595148 Start of integration
#    2   obc_mag_i            mag          Real4          -Inf:+Inf            OBC est. image instr. magn.
#    3   obc_ang_y            deg          Real4          -Inf:+Inf            OBC centroid angle in ACA y
#    4   obc_ang_z            deg          Real4          -Inf:+Inf            OBC centroid angle in ACA z
#    5   bkg_rms              adu          Real4          -Inf:+Inf            Bacgkround RMS
#    6   bkg_avg              adu          Real4          -Inf:+Inf            Average background
#    7   aca_comp_bkg_avg     count        Real4          -Inf:+Inf            Compensated average bkgd (e-)
#    8   img_raw[6,6]         adu          Real4(6x6)     -Inf:+Inf            Raw image
#    9   img_corr[6,6]        count        Real4(6x6)     -Inf:+Inf            Corrected image (e-)
#   10   fit_resid[6,6]       count        Real4(6x6)     -Inf:+Inf            PSF or gauss fit residuals (e-)
#   11   temperatures[4]      K            Real4(4)       -Inf:+Inf            Satellite Temperatures
#   12   cent_i_true          pixel        Real4          -Inf:+Inf            True centroid from GADSOOCS
#   13   cent_j_true          pixel        Real4          -Inf:+Inf            True centroid from GADSOOCS
#   14   q_sc_true[4]                      Real8(4)       -Inf:+Inf            True S/C attitude quaternion
#   15   cmd_count                         Int4           -                    Command count
#   16   cmd_progress                      Int4           -                    Command progress
#   17   img_row0             pixel        Int2           -                    Row pos of lower left pixel
#   18   img_col0             pixel        Int2           -                    Col pos of lower left pixel
#   19   img_excl[6,6]                     Byte(6x6)                           (BYTE) Exclude pixel flag
#   20   img_func_stat                     Byte                                (BYTE) Image funct. and status bits
#   21   global_status                     Byte                                (BYTE) global status
#   22   bkg_outliers                      Byte                                (BYTE) bgd outliers that got tossed

