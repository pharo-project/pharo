printing-numerative
printStringHex
	"returns the hex digit part of the integer when printed in hexadecimal format.
	30 printStringHex 
		'1E'
	30 hex
		 '16r1E' "
		
	^self printStringBase: 16