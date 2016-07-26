I am an alternate to EncoderForV3 that tries to use thje longest forms of bytecodes possible so as to avoid using as many bytecode as possible to allow for the unused portions of the bytecode set this makes available to be reassigned.



I do not use the following ranges

0 through 111

	   0- 15 	0000iiii 	Push Receiver Variable #iiii

	  16- 31 	0001iiii 	Push Temporary Location #iiii

	  32- 63 	001iiiii 		Push Literal Constant #iiiii

	  64- 95 	010iiiii 		Push Literal Variable #iiiii

	  96-103 	01100iii 	Pop and Store Receiver Variable #iii

	104-111 	01101iii 	Pop and Store Temporary Location #iii

138-159

	138-143 				Unused.

	144-151 	10010iii 	Jump iii + 1 (i.e., 1 through 8).

	152-159 	10011iii 	Pop and Jump 0n False iii +1 (i.e., 1 through 8).

176-255

	176-191 	1011iiii 	Send Arithmetic Message #iiii

	192-207 	1100iiii 	Send Special Message #iiii

	208-223 	1101iiii 	Send Literal Selector #iiii With No Arguments

	224-239 	1110iiii 	Send Literal Selector #iiii With 1 Argument

	240-255 	1111iiii 	Send Literal Selector #iiii With 2 Arguments

= 112 + (160 - 138) + (256 - 176) =  214, or 84% of the bytecodes