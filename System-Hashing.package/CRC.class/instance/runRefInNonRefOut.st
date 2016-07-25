private-run-methods
runRefInNonRefOut
	Warning signal: 'unverified implementation'.
	theRegister := registerFill.
	1 to: message size do: [ :byteIndex | 
		theRegister := 
			(lookupTable at: ((theRegister bitXor: (message byteAt: byteIndex)) bitAnd: 255) + 1) 
			bitXor: ((theRegister bitShift: 8) bitAnd: self widthMask) ].
			
	^ finalXorBytes bitXor: theRegister