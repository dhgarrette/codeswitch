package Statistics;

# Elizabeth Blair
# Last Edited: 4/30/14
# 4/30/14:	Created

use strict;
use warnings;
use v5.10;

# A set of operations to compute various statistical evaluation measures and various math.
# If a calculation would divide by zero, it instead returns a 0 as its output result.

# factorial()
# Input: int to take factorial of
# Output: int factorial
# Description: Calculate and return the factorial of the given integer.
sub factorial
{
	my $ret = 1;
	for (my $i = shift; $i > 1; $i--) { $ret *= $i; }
	return $ret;
}

# combination()
# Input: int n, int k
# Output: int combination
# Description: Calculate and return the combination of n things taken k at a time.
# 		C(n,k) = n!/(k!*(n-k)!)
sub combination
{
	my ($n,$k) = @_;
	my $ret = (factorial($n)/(factorial($k)*factorial($n-$k)));
	if ($ret < 1) { $ret = 0; }
	return $ret;
}

# accuracy()
# Input: (int counts) true positive, true negative, positive, negative
# Output: (double) accuracy measure
# Description: Given the counts for true positives and negatives, and all positives and negatives,
#		calculate and return the accuracy.
#		acc = (tp+tn)/(p+n)
sub accuracy
{
	my ($tp,$tn,$p,$n) = @_;
	if ($p+$n == 0) { return 0; }
	return ($tp+$tn)/($p+$n);
}

# precision()
# Input: (int counts) true positive, false positive
# Output: (double) precision measure
# Description: Given the counts for true and false positives, calculate and return the precision.
#		prec = tp/(tp+fp)
sub precision
{
	my ($tp,$fp) = @_;
	if ($tp+$fp == 0) { return 0; }
	return $tp/($tp+$fp);
}

# recall()
# Input: (int counts) true positive, false negative
# Output: (double) recall measure
# Description: Given the counts for true positives and false negatives, calculate and return 
#		the recall.
#		rec = tp/(tp+fn)
sub recall
{
	my ($tp,$fn) = @_;
	if ($tp+$fn == 0) { return 0; }
	return $tp/($tp+$fn);
}

# fmeasure()
# Input: (doubles) precision, recall
# Output: (double) F-measure
# Description: Given the precision and recall, calculate and return the F-Measure.
#		F = 2*((prec*rec)/(prec+rec))
sub fmeasure
{
	my ($prec,$rec) = @_;
	if ($prec+$rec == 0) { return 0; }
	return 2*(($prec*$rec)/($prec+$rec));
}

1;
