My instances represent IEEE-754 floating-point double-precision numbers.  They have about 16 digits of accuracy and their range is between plus and minus 10^307. Some valid examples are:
	
	8.0 13.3 0.3 2.5e6 1.27e-30 1.27e-31 -12.987654e12

Mainly: no embedded blanks, little e for tens power, and a digit on both sides of the decimal point.  It is actually possible to specify a radix for Float constants.  This is great for teaching about numbers, but may be confusing to the average reader:

	3r20.2 --> 6.66666666666667
	8r20.2 --> 16.25

If you don't have access to the definition of IEEE-754, you can figure out what is going on by printing various simple values in Float hex.  It may help you to know that the basic format is...
	sign		1 bit
	exponent	11 bits with bias of 1023 (16r3FF) to produce an exponent
						in the range -1023 .. +1024
				- 16r000:
					significand = 0: Float zero
					significand ~= 0: Denormalized number (exp = -1024, no hidden '1' bit)
				- 16r7FF:
					significand = 0: Infinity
					significand ~= 0: Not A Number (NaN) representation
	mantissa	53 bits, but only 52 are stored (20 in the first word, 32 in the second).  This is because a normalized mantissa, by definition, has a 1 to the right of its floating point, and IEEE-754 omits this redundant bit to gain an extra bit of precision instead.  People talk about the mantissa without its leading one as the FRACTION, and with its leading 1 as the SIGNFICAND.

The single-precision format is...
	sign		1 bit
	exponent	8 bits, with bias of 127, to represent -126 to +127
                    - 0x0 and 0xFF reserved for Float zero (mantissa is ignored)
                    - 16r7F reserved for Float underflow/overflow (mantissa is ignored)
	mantissa	24 bits, but only 23 are stored
This format is used in FloatArray (qv), and much can be learned from the conversion routines, Float asIEEE32BitWord, and Float class fromIEEE32Bit:.

Thanks to Rich Harmon for asking many questions and to Tim Olson, Bruce Cohen, Rick Zaccone and others for the answers that I have collected here.