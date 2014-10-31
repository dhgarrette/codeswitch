#!/usr/bin/perl

use strict;
use warnings;
use v5.10;

use FindBin;
use lib "$FindBin::Bin/../Libraries";
use Statistics;

# Elizabeth Blair
# Last Edited: 5/7/14
# 5/1/14:	Added monolingual measures and cleaned up formatting
# 5/7/14:	Name changed; cleaned up/checks added for release

# Generate statistical results for the data file when testing it against the test (gold)
# file. The measures generated are tweet-level monolingual versus nonmonolingual analysis,
# token-level annotation analysis, and the token-level annotation confusion table. In the
# first two sections, the measures given are accuracy, precision, recall, and F-measure.

# If a tweet in the gold file is not present in the evaluation file, then it is not counted
# in the results. 

# Input files (data and test) must be in shared task TSV offset format.
# The output is in CSV format with " surrounding most fields.

# Usage: ./evaluateOffsets.pl <data file> <gold test file> <result output file>

# Note: Data file is the file to evaluate and gold test file is the gold to check the data against.

if (scalar(@ARGV) != 3) { die "Invalid number of arguments: provide evaluation, gold, and output files"; }
my ($dataFile,$testFile,$confFile) = @ARGV;
open(my $dataFileHan,'<:encoding(utf8)',$dataFile) or die "Can't open input file $dataFile";
open(my $testFileHan,'<:encoding(utf8)',$testFile) or die "Can't open input file $testFile";
open(my $confFileHan,'>:encoding(utf8)',$confFile) or die "Can't open output file $confFile";

if (eof($dataFileHan) or eof($testFileHan)) { die "Data or gold file is empty - please check"; }

# Gather and store information for each data tweet
my %tweetTags;
# Read in and save the content for the first line of the file
my $firstLine = <$dataFileHan>;
chomp $firstLine;
my ($curTweet,@saved) = split(/\t/,$firstLine);
while(!eof($dataFileHan))
{	# Pull in the next tweet's offsets and store relevant data about it
	my @offsets;		# Stores this tweet's offsets
	my @annotations;	# Stores this tweet's annotations (1-1 with offsets)
	my ($tweet,$user); 	# Stores the tweet and user IDs for the current tweet
	my ($lang1,$lang2);	# Booleans to register if a lang1 or lang2 have been seen

	# Pull any saved information into the current tweet's data
	if (@saved)
	{
		$tweet = $curTweet;
		$user = shift(@saved);
		push(@offsets,[$saved[0],$saved[1]]);
		push(@annotations,$saved[2]);
		if 	($saved[2] eq 'lang1') { $lang1 = 1; }
		elsif 	($saved[2] eq 'lang2') { $lang2 = 1; }
		@saved = undef;
	}

	# Read offset lines and include them in the current tweet's data until the tweet ID changes.
	# When that happens, save that line's data for the next iteration and leave the loop.
	while(<$dataFileHan>)
	{
		chomp;
		my @parts = split(/\t/,$_);
		if ($curTweet and $curTweet == $parts[0])
		{
			unless ($user == $parts[1]) { die "Mismatched users in same tweet: $curTweet"; };
			push(@offsets,[$parts[2],$parts[3]]);
			push(@annotations,$parts[4]);
			if 	($parts[4] eq 'lang1') { $lang1 = 1; }
			elsif 	($parts[4] eq 'lang2') { $lang2 = 1; }
		}
		else
		{
			$curTweet = shift(@parts);
			@saved = @parts;
			last;
		}
	}
	# Store this tweet's user, offsets, annotations, and monolingual status (1 or 0)
	$tweetTags{$tweet}{user} 	= $user;
	$tweetTags{$tweet}{offsets} 	= \@offsets;
	$tweetTags{$tweet}{annos} 	= \@annotations;
	$tweetTags{$tweet}{mono} 	= ($lang1 and $lang2) ? 1 : 0;
}
undef $firstLine; undef @saved; undef $curTweet;

# Match up and compare the test data to the evaluation data
my (%confData,%monoData);		# Storage for confusion and monolingual data
my ($totalToken,$totalTweet) = (0,0);	# Counters for total # of tokens and tweets
# Read in and save the content for the first line of the file
$firstLine = <$testFileHan>;
chomp $firstLine;
($curTweet,@saved) = split(/\t/,$firstLine);
while(!eof($testFileHan))
{	# Pull the next tweet's offsets
	my @offsets;		# Stores this tweet's offsets
	my @annotations;	# Stores this tweet's annotations
	my ($tweet,$user); 	# Stores the tweet and user IDs for the current tweet
	my ($lang1,$lang2);	# Booleans to register if a lang1 or lang2 have been seen

	# Pull any saved information into the current tweet's data
	if (@saved)
	{
		$tweet = $curTweet;
		$user = shift(@saved);
		push(@offsets,[$saved[0],$saved[1]]);
		push(@annotations,$saved[2]);
		if 	($saved[2] eq 'lang1') { $lang1 = 1; }
		elsif 	($saved[2] eq 'lang2') { $lang2 = 1; }
		@saved = undef;
	}

	# Read offset lines and include them in the current tweet's data until the tweet ID changes.
	# When that happens, save that line's data for the next iteration and leave the loop.
	while(<$testFileHan>)
	{
		chomp;
		my @parts = split(/\t/,$_);
		if ($curTweet and $curTweet == $parts[0])
		{
			unless ($user == $parts[1]) { die "Mismatched users in same tweet: $curTweet"; };
			push(@offsets,[$parts[2],$parts[3]]);
			push(@annotations,$parts[4]);
			if 	($parts[4] eq 'lang1') { $lang1 = 1; }
			elsif 	($parts[4] eq 'lang2') { $lang2 = 1; }
		}
		else
		{
			$curTweet = shift(@parts);
			@saved = @parts;
			last;
		}
	}

	# Gather statistical data on the tweet and its tokens compared to the data being evaluated
	# Only records results for tweets that exist in the data file - if a tweet is in the gold
	# but not the data, then it is not included in the results.
	if (exists($tweetTags{$tweet}))
	{
		my @dataOffsets = @{$tweetTags{$tweet}{offsets}};
		my @dataAnnos 	= @{$tweetTags{$tweet}{annos}};

		# Record the tweet's monolingual status (confusion table counts)
		$totalTweet++;
		my $mono = ($lang1 and $lang2) ? 1 : 0;
		$monoData{$mono}{$tweetTags{$tweet}{mono}}++;

		# For each offset, record the confusion table count for the annotation's status
		# Abort if either of the offsets in gold doesn't match counterpart in data
		for (my $i = 0; $i < scalar(@offsets); $i++)
		{
			if ($dataOffsets[$i][0] != $offsets[$i][0] or $dataOffsets[$i][1] != $offsets[$i][1])
				{ die "Mismatched offsets for tweet $tweet on original off $offsets[$i][0] 
					$offsets[$i][1], data off $dataOffsets[$i][0] $dataOffsets[$i][1]"; }
			$totalToken++;
			$confData{$annotations[$i]}{$dataAnnos[$i]}++;
		}
	}
}

# Make sure there's an entry in every location the statistical checks may look at
foreach my $gold (keys(%confData))
{
	foreach my $test (keys(%confData))
	{
		unless (exists($confData{$gold}{$test})) { $confData{$gold}{$test} = 0; }
	}
}
foreach my $gold (keys(%monoData))
{
	foreach my $test (keys(%monoData))
	{
		unless (exists($monoData{$gold}{$test})) { $monoData{$gold}{$test} = 0; }
	}
}

# Get the monolingual statistical measures from the count data.
my @monoScores;
$monoScores[0] = Statistics::accuracy($monoData{1}{1},$monoData{0}{0},$monoData{1}{1}+$monoData{0}{1},$monoData{0}{0}+$monoData{1}{0});
$monoScores[1] = Statistics::precision($monoData{1}{1},$monoData{0}{1});
$monoScores[2] = Statistics::recall($monoData{1}{1},$monoData{1}{0});
$monoScores[3] = Statistics::fmeasure($monoScores[1],$monoScores[2]);

# Get the various statistical measures for each annotation class from the token count data.
my %scores;
my @catsToCheck = keys(%confData);
foreach my $cat (@catsToCheck)
{
	my ($tp,$fp,$tn,$fn);
	foreach my $gold (keys(%confData))
	{
		foreach my $test (keys(%confData))
		{
			if ($cat eq $gold and $cat eq $test)	{ $tp += $confData{$gold}{$test}; }
			elsif ($cat eq $gold)			{ $fn += $confData{$gold}{$test}; }
			elsif ($cat eq $test)			{ $fp += $confData{$gold}{$test}; }
			else					{ $tn += $confData{$gold}{$test}; }
		}
	}

	$scores{$cat}[0] = Statistics::accuracy($tp,$tn,($tp+$fp),($tn+$fn));
	$scores{$cat}[1] = Statistics::precision($tp,$fp);
	$scores{$cat}[2] = Statistics::recall($tp,$fn);
	$scores{$cat}[3] = Statistics::fmeasure($scores{$cat}[1],$scores{$cat}[2]);
}

# Normalize the token counts with the total number of tokens
foreach my $gold (keys(%confData))
{
	foreach my $test (keys(%{$confData{$gold}}))
	{
		$confData{$gold}{$test} = $confData{$gold}{$test}/$totalToken;
	}
}

# Print out the statistics and confusion data
say $confFileHan 'Tweet-Level Monolingual Results';
say $confFileHan "Accuracy,\"$monoScores[0]\"";
say $confFileHan "Precision,\"$monoScores[1]\"";
say $confFileHan "Recall,\"$monoScores[2]\"";
say $confFileHan "F-Measure,\"$monoScores[3]\"";
say $confFileHan '';

say $confFileHan 'Token-Level Annotation Results';
say $confFileHan '"","'.join('","',sort(keys(%confData))).'"';
my @outData;
foreach(sort(keys(%confData))) { push(@outData,$scores{$_}[0]); }
say $confFileHan 'Accuracy,"'.join('","',@outData).'"';
undef @outData;
foreach(sort(keys(%confData))) { push(@outData,$scores{$_}[1]); }
say $confFileHan 'Precision,"'.join('","',@outData).'"';
undef @outData;
foreach(sort(keys(%confData))) { push(@outData,$scores{$_}[2]); }
say $confFileHan 'Recall,"'.join('","',@outData).'"';
undef @outData;
foreach(sort(keys(%confData))) { push(@outData,$scores{$_}[3]); }
say $confFileHan 'F-Measure,"'.join('","',@outData).'"';
say $confFileHan '';

say $confFileHan 'Token-Level Confusion Matrix';
say $confFileHan '"Gold","Test"';
say $confFileHan '"","'.join('","',sort(keys(%confData))).'"';
foreach my $goldAnno (sort(keys(%confData)))
{	
	my @outparts;
	push(@outparts,$goldAnno);
	if (exists($confData{$goldAnno}))
	{
		foreach my $testAnno (sort(keys(%confData)))
		{
			if (exists($confData{$goldAnno}{$testAnno}))	{ push (@outparts,$confData{$goldAnno}{$testAnno}); }
			else  						{ push (@outparts,0); }
		}
	}
	say $confFileHan '"'.join('","',@outparts).'"';
}

# Close files
close($dataFileHan) or die "Can't close $dataFile";
close($testFileHan) or die "Can't close $testFile";
close($confFileHan) or die "Can't close $confFile";
