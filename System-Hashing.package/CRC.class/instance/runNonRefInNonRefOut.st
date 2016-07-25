private-run-methods
runNonRefInNonRefOut
	theRegister := registerFill.
	1 to: message size do: [ :byteIndex | 
		theRegister := 
			(lookupTable at: (((theRegister bitShift: 0 - self lowestByteShift) bitXor: (message byteAt: byteIndex)) bitAnd: 255) + 1)
			bitXor: ((theRegister bitShift: 8) bitAnd: self widthMask) ].
			
	^ finalXorBytes bitXor: theRegister