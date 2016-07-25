private-run-methods
runRefInRefOut
	theRegister := registerFill.
	1 to: message size do: [ :byteIndex | 
		theRegister := 
			(lookupTable at: ((theRegister bitXor: (message byteAt: byteIndex)) bitAnd: 255) + 1) 
			bitXor: (theRegister bitShift: -8) ].
			
	^ finalXorBytes bitXor: theRegister