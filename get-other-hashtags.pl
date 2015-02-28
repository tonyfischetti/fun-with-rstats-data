#!/usr/bin/perl

while(<>){
    chomp;
    $_ = lc($_);
    $_ =~ s/#rstats//g;
    my @matches;
    push @matches, /(#\w+)/;
    print join "\n" => @matches if @matches;
}

