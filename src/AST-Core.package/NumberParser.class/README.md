NumberParser is an abstract class for parsing and building numbers from string/stream.
It offers a framework with utility methods and exception handling.

Number syntax is not defined and should be subclassResponsibility.

Instance variables:
sourceStream <Stream> the stream of characters from which the number is read
base <Integer> the radix in which to interpret digits
neg <Boolean> true in case of minus sign
integerPart <Integer> the integer part of the number
fractionPart <Integer> the fraction part of the number if any
exponent <Integer> the exponent used in scientific notation if any
scale <Integer> the scale used in case of ScaledDecimal number if any
nDigits <Integer> number of digits read to form an Integer
lasNonZero <Integer> position of last non zero digit, starting at 1 from left, 0 if all digits are zero
requestor <?> could eventually be used to insert an error message in a text editor
failBlock <BlockClosure> Block to execute whenever an error occurs