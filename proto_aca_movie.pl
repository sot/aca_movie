#!/usr/bin/env perl

use warnings;
use PDL;
use PDL::Graphics::PGPLOT::Window;
use PDL::NiceSlice;
use PDL::ImageND;
use CFITSIO::Simple;
use Time::HiRes qw(sleep);
use Getopt::Long;

%opt = ( slot => "0 1 2 3 4 5 6 7",
	 delay => 0.001,
	 raw   => 0,
       );

GetOptions(\%opt,
	   "slot=s",
	   "delay=f",
	   "raw!",
	   );

$MAX_DRC = 14;
$MIN_LOW = 1.0;
@slot = split ' ',$opt{slot};

foreach $s (@slot) {
    # Find files for this slot
    @file = sort(@ARGV ? grep(/(adat${s}1.fits|${s}_img0.fits)/, @ARGV)
		 : glob "pcad*adat${s}1.fits* acaf*${s}_img0.fits*");
    next unless @file;

    # Initialize data and limit variables
    ($r0[$s], $r1[$s], $c0[$s], $c1[$s]) = (2000,-2000,2000,-2000);
    $img[$s] = null;
    $row[$s] = null;
    $col[$s] = null;
    $time[$s] = null;
    $indx[$s] = null;
    $size[$s] = null;

    # Read each file and append to main data piddles
    foreach $file (@file) {
	print "Processing file $file\n";
	if ($file =~ /_img0.fits/) {
	    $img_row0 = 'imgrow0';
	    $img_col0 = 'imgcol0';
	    $img_data = 'imgraw';
	} else {
	    $img_row0 = 'img_row0';
	    $img_col0 = 'img_col0';
	    $img_data = $opt{raw} ? 'img_raw' : 'img_corr';
	}
	    
	%dat = fits_read_bintbl($file, 'time', $img_row0, $img_col0, $img_data);
	$nelem = $dat{$img_row0}->nelem();
	($sz) = dims $dat{$img_data};
	$size[$s] = append($size[$s], $sz + zeroes(short, $nelem));
	$indx[$s] = append($indx[$s], sequence($nelem)*$sz**2 + $img[$s]->nelem());
	$img[$s] = append($img[$s], $dat{$img_data}->flat);
	$row[$s] = append($row[$s], $dat{$img_row0}->flat);
	$col[$s] = append($col[$s], $dat{$img_col0}->flat);
	$time[$s] = append($time[$s], $dat{time}->flat);
    }

    # Set min and max time for each slot
    ($t0[$s], $t1[$s]) = minmax($time[$s]);

    # Calculate statistics on image for scaling purposes
    $img_5th = $img[$s]->pct(0.05);
    $img_5th = -10 if $img_5th < -10;
    $img_95th = $img[$s]->pct(0.95);
    if ($img_5th < $MIN_LOW) {
	my $img_off = $MIN_LOW - $img_5th;
	$img[$s] += $img_off;
	$img_95th += $img_off;
    }
    # For log scaling, make sure lower bound is positive
    $img[$s] = $img[$s] * ($img[$s] > $MIN_LOW) + $MIN_LOW * ($img[$s] <= $MIN_LOW);
    $img[$s] = log10($img[$s]) * 3.0 / log10($img_95th);
    
    # Make image back into a 3-d array (size x size x n_images)

#    ($sz) = dims $dat{$img_data};
#    $img[$s]->reshape($sz, $sz, $img[$s]->nelem/($sz**2));
#    $sz[$s] = $sz;

    # Add image size to row,col maxima
    $r1[$s] += $sz;
    $c1[$s] += $sz;
}

# Make @slot so it only contains valid values
@slot = grep { defined $img[$_] } (0..7) or die "No data!\n";

# Set global min and max of time
$t0 = min(pdl [ map { $t0[$_] } @slot ]);
$t1 = max(pdl [ map { $t1[$_] } @slot ]);
print "Min and max time = $t0, $t1  \(Delta = ", $t1-$t0, ")\n";

# Global max of d_row and d_col
$dr = $MAX_DRC;
$dc = $MAX_DRC;

# Find correct indices for each time slice
$n_t = int(($t1-$t0)/1.025);
$t = $t0 + sequence($n_t) * 1.025 + 0.25;
map { $ind[$_] = vsearch($t, $time[$_]) } @slot;

# Do the display

$win = PDL::Graphics::PGPLOT::Window->new({Dev=>'/xserve', nx=>1,
					   ny=>1, AspectRatio=>0.25,
					   axis=>EMPTY,
					   WindowWidth => 10,
					  });

for $i (0..$n_t-1) {
    $image = zeroes(float, $dc*8, $dr);
    $image->hdrcpy(0);
    $all_idle = 1;

    for $s (@slot) {
	# Determine where in all-slots image the individual slot image should go
	$ii = $ind[$s]->at($i);
	$sz = $size[$s]->at($ii);
	$sc0 = $col[$s]->at($ii) - $c0[$s];
	$sc1 = $sc0 + $sz - 1;
	$sr0 = $row[$s]->at($ii) - $r0[$s];
	$sr1 = $sr0 + $sz - 1;
	
	# Check limits
	if ($sc0 < 0) {
	    print "Changing co[$s] from $c0[$s] to ";
	    $c0[$s] = $col[$s]->at($ii);
	    print "$c0[$s]\n";
	} elsif ($sc1 >= $dc) {
	    print "Changing co[$s] from $c0[$s] to ";
	    $c0[$s] = $col[$s]->at($ii) + $sz - $dc;
	    print "$c0[$s]\n";
	}

	if ($sr0 < 0) {
	    print "Changing ro[$s] from $r0[$s] to ";
	    $r0[$s] = $row[$s]->at($ii);
	    print "$r0[$s]\n";
	} elsif ($sr1 >= $dr) {
	    print "Changing ro[$s] from $r0[$s] to ";
	    $r0[$s] = $row[$s]->at($ii) + $sz - $dr;
	    print "$r0[$s]\n";
	}
	    
	# recalculate since $c0 or $r0 may have changed.  Also add in column offset
	$sc0 = $col[$s]->at($ii) - $c0[$s] + $s * $dc;
	$sc1 = $sc0 + $sz - 1;
	$sr0 = $row[$s]->at($ii) - $r0[$s];
	$sr1 = $sr0 + $sz - 1;

	# Copy the individual image

#  $img[$s]->reshape($sz, $sz, $img[$s]->nelem/($sz**2));
	$img_current = $img[$s]->( $indx[$s]->at($ii) : $indx[$s]->at($ii) + $sz**2-1);
	$img_current->reshape($sz, $sz);

#	$image($sc0:$sc1, $sr0:$sr1) .= $img[$s]->(:,:,$ii;-);
	$image($sc0:$sc1, $sr0:$sr1) .= $img_current;

	$all_idle = 0 if ($row[$s]->at($ii) != 511 or $col[$s]->at($ii) != 511);
#	print "slot $s ", $row[$s]->at($ii), " ", $size[$s]->at($ii), " ", $indx[$s]->at($ii), "\n";
    }
    next if $all_idle;
#    print $t->at($i),"\n";
#    $image_big *= 0;
#    $image->rescale2d($image_big);
#    print "hey: ", $image_big->dims(), "\n";
    @dims = $image->dims();
    $win->imag(rebin($image, $dims[0]*7, $dims[1]*7), {pix=>1, scale=>1}); # {scale=>1, pix=>1, min=>0, max=>3}
    $win->hold();
    sleep $opt{delay};
}

