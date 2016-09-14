#!perl -T
use 5.006;
use strict;
use warnings FATAL => 'all';
use Test::More;



use String::Calc;

my $amount;

$amount = String::Calc->new( '13.369,51 EUR' );   is( $amount . "", '13.369,51 EUR', 'a  = 13.369,51 EUR ; a == 13.369,51 EUR' );

$amount += '-9.900,00 EUR';                       is( $amount . "", '3.469,51 EUR',  'a += -9.900,00 EUR ; a ==  3.469,51 EUR' );
$amount +=      '6,22 EUR';                       is( $amount . "", '3.475,73 EUR',  'a +=      6,22 EUR ; a ==  3.475,73 EUR' );
$amount +=    '500,00 EUR';                       is( $amount . "", '3.975,73 EUR',  'a +=    500,00 EUR ; a ==  3.975,73 EUR' );
$amount +=    '500,00 EUR';                       is( $amount . "", '4.475,73 EUR',  'a +=    500,00 EUR ; a ==  4.475,73 EUR' );
$amount -=  '4.475,73 EUR';                       is( $amount . "",     '0,00 EUR',  'a -=  4.475,73 EUR ; a ==      0,00 EUR' );

$amount +=  '4.475,73 EUR';                       is( $amount . "", '4.475,73 EUR',  'a +=  4.475,73 EUR ; a ==  4.475,73 EUR' );

$amount += '-4.000,00 EUR';                       is( $amount . "",   '475,73 EUR',  'a -=  4.000,00 UER ; a ==    475,73 EUR' );
$amount +=   '-475,00 EUR';                       is( $amount . "",     '0,73 EUR',  'a -=    475,00 UER ; a ==      0,73 EUR' );
$amount +=     '-0,73 EUR';                       is( $amount . "",     '0,00 EUR',  'a -=      0,73 UER ; a ==      0,00 EUR' );

done_testing();
