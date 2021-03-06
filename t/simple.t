#!perl -T
use 5.006;
use strict;
use warnings FATAL => 'all';
use Test::More;

plan tests => 6;


use String::Calc;

my $amount = String::Calc->new( '12.345.678,90 EUR' );

is( ($amount + $amount)."",      '24.691.357,80 EUR', 'a+a => 2a' );
is( ($amount * 10)."" ,         '123.456.789,00 EUR', 'a*10 => 10a' );
is( ($amount + '')."",           '12.345.678,90 EUR', 'a+0 => a' );
is( ($amount + undef)."",        '12.345.678,90 EUR', 'a+undef => a' );
is( ($amount + '321,10 EUR')."", '12.346.000,00 EUR', 'a+b => (a+b)' );

$String::Calc::suffix->{'apples'} = {
};

my $apples = String::Calc->new( '5 apples' );

is ( ($apples)."", '5 apples', '5 apples == 5 apples' );

done_testing();
