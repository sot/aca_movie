#!/usr/bin/env /proj/axaf/bin/perlwrap

use warnings;
use PDL;
use PDL::Graphics::PGPLOT::Window;
use PDL::NiceSlice;
use PDL::ImageND;
use Time::HiRes qw(sleep);
use Getopt::Long;
use Carp;
use Data::Dumper;

my ($MAX_DRC, $MIN_LOW);
my @slot;

$MAX_DRC = 14;			# Size in pixels of window containing image data
$MIN_LOW = 1.0;			# Minimum pixel value
$DT      = 2.05;		# default time step in movie
$BIG_TIME = 1e14;		# Large time

%opt = ( slot => "0 1 2 3 4 5 6 7",
	 delay => 0.001,
	 raw   => 0,
	 loud => 0,
	 tstart => 0,
	 tstop  => $BIG_TIME,
	 dt     => $DT,
       );

GetOptions(\%opt,
	   "slot=s",
	   "delay=f",
	   "raw!",
	   "tstart=f",
	   "tstop=f",
	   "loud!",
	   "dt=f",
	   );

$Slot::loud = $opt{loud};
$loud = $opt{loud};

($data_dir = shift @ARGV) and (chdir $data_dir or croak "Could not find directory $data_dir\n");

# For each specified slot, create a new slot object.  This reads in the list
# of files corresponding to each slot and sets the global start and stop time
# of all files for that slot

@slot = split ' ',$opt{slot};
$tstart = $BIG_TIME;
$tstop  = 0;

foreach $s (@slot) {
    next unless ($slts[$s] = new Slot (slot => $s));
    $tstart = $slts[$s]->{tstart} if $slts[$s]->{tstart} < $tstart;
    $tstop = $slts[$s]->{tstop} if $slts[$s]->{tstop} > $tstop;
}

# Now show the movie of images corresponding to the entire range of
# selected files, or a subset if specified on command line

$tstart = $opt{tstart} if $opt{tstart} > $tstart;
$tstop  = $opt{tstop}  if $opt{tstop}  < $tstop;

# Create the display

$win = PDL::Graphics::PGPLOT::Window->new({Dev=>'/xserve', nx=>1,
					   ny=>1, AspectRatio=>(1/7.),
					   axis=>EMPTY,
					   WindowWidth => 10,
					  });

$| = 1;
$time   = ($opt{dt} > 0) ? $tstart : $tstop;

while ($time <= $tstop and $time >= $tstart) {
    $image = zeroes(float, $MAX_DRC*8, $MAX_DRC);
    $image->hdrcpy(0);
    $all_idle++;
    printf ("time = %.2f" . ($loud ? "\n" : "\r"), $time);

    for $s (@slot) {
	next unless ($slt = $slts[$s]);
	next unless ($img = $slt->get_image($time));

	print "Processing slot $s\n" if $loud;

	# Determine where in all-slots image the individual slot image should go
	$sz = $slt->{file}{sz};

	$sc0 = $img->{col0} - $slt->{c0};
	$sc1 = $sc0 + $sz - 1;
	$sr0 = $img->{row0} - $slt->{r0};
	$sr1 = $sr0 + $sz - 1;
	
	# Check limits
	if ($sc0 < 0) {
	    $slt->{c0} = $img->{col0};
	} elsif ($sc1 >= $MAX_DRC) {
	    $slt->{c0} = $img->{col0} + $sz - $MAX_DRC;
	}
	if ($sr0 < 0) {
	    $slt->{r0} = $img->{row0};
	} elsif ($sr1 >= $MAX_DRC) {
	    $slt->{r0} = $img->{row0} + $sz - $MAX_DRC;
	}
	    
	# recalculate since $c0 or $r0 may have changed.  Also add in column offset
	$sc0 = $img->{col0} - $slt->{c0} + $s * $MAX_DRC;
	$sc1 = $sc0 + $sz - 1;
	$sr0 = $img->{row0} - $slt->{r0};
	$sr1 = $sr0 + $sz - 1;

	# Copy the individual image
	$image($sc0:$sc1, $sr0:$sr1) .= $img->{img};
	print "slot: $s row: $img->{row0}  col:  $img->{col0}\n" if $loud;
	print $img->{img} if $opt{loud};

	$all_idle = 0 if ($img->{row0} != 511 or $img->{col0} != 511);
	print "slot $s ", $img->{row0}, " ", $img->{col0}, " ", $sz, " \n" if $loud;
    }
    $time += $opt{dt};
    next if $all_idle > 2;

    @dims = $image->dims();
    $win->imag(rebin($image, $dims[0]*7, $dims[1]*7), {pix=>1, scale=>1, min=>0, max=>5}); 
    $win->hold();
    sleep $opt{delay};

}

print "\nDone\n";


package Slot;

use PDL;
use PDL::NiceSlice;
use CFITSIO::Simple;
use Carp;
use Data::Dumper;

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
		 @_
	       );

    # Initialize the list of appropriate L0 or L1 ACA image files
    unless (@{$self->{file_list}}) {
	my $l0_glob = "aca*$self->{slot}_img0.fits*";
	my $l1_glob = "pcad*adat$self->{slot}?.fits*";
	@files = glob($l0_glob);
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
	    $self->set_plot_limits(); # Set plotting limits (brightness and coordinates)
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
	    $data{img} = $self->{use_raw} ? $data{img_raw} : $data{img_corr}/5;
	    $data{row0} = $data{img_row0};
	    $data{col0} = $data{img_col0};
	} else {
	    $data{img} = $data{imgraw};
	    $data{row0} = $data{imgrow0};
	    $data{col0} = $data{imgcol0};
	}

	# Create an info record for this file and store 
	my $dt = $data{time}->at(1) - $data{time}->at(0); # uff, should use hdr
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
		    };
	
	$self->{file_info}{$name} = $info;

	# If the desired time is in this file, return success!
	if ($tstart <= $time && $time < $tstop) {
	    $self->{file} = $info;
	    $self->set_plot_limits(); # Set plotting limits (brightness and coordinates)
	    return 1;
	}
    }

    return;
}

##****************************************************************************
sub set_plot_limits {
#
# Set object variables which control brightness scaling and
# CCD region corresponding to first image
#
##****************************************************************************
    my $self = shift;
    my $xvals;
    my $MIN_LOW = 1.0;
    my $sz  = $self->{file}{sz};
    $self->{last_sz} = $self->{sz};
    $self->{sz} = $sz;

    # For log scaling, make sure lower bound is positive
    my $img = $self->{file}{data}{img} + $MIN_LOW + 5.0;
    $self->{file}{data}{img} = log10($img * ($img > $MIN_LOW) + $MIN_LOW * ($img <= $MIN_LOW));

    # Generate histogram of pixel values, add this to any existing histogram, and
    # then make the cumulative histogram
    ($xvals, $self->{file}{hist}) = hist($self->{file}{data}{img}, 0, 5, 0.005);

    $self->{hist} = zeroes($self->{file}{hist}) unless (exists $self->{hist} and $sz == $self->{last_sz});
    $self->{hist} += $self->{file}{hist};
    my $cumsum = cumusumover $self->{hist};

    # Get the index of the 95th percentile
    my $ok = which ($cumsum > 0.95*$cumsum->at(-1));
    $self->{img_95th} = $xvals($ok)->at(0);
    print "img_95th for slot $self->{slot} = $self->{img_95th}\n" if $loud;
    $self->{file}{data}{img} *= 5.0 / $self->{img_95th};
    
    # Set row,col limits if not already set.  (Normally these variables are
    # manipulated in the plotting itself)
    unless (exists $self->{r0} and exists $self->{c0}) {
	$self->{r0} =  sprintf("%d", $self->{file}{data}{row0}->at(0) + $sz/2 - $MAX_DRC/2);
	$self->{c0} =  sprintf("%d", $self->{file}{data}{col0}->at(0) + $sz/2 - $MAX_DRC/2);
    }
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
