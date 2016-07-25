private
setBit: bit value: aBoolean
	
	buttons := buttons - (buttons bitAnd: bit) + (aBoolean ifTrue: [ bit  ] ifFalse: [ 0 ])