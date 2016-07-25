accessing
fromArray: anArray
	| str |
	str := anArray readStream.
	
	#( leftFraction: topFraction: rightFraction: bottomFraction: leftOffset: topOffset: rightOffset: bottomOffset: ) do: [:sel |
		str next ifNil: [ ^ self ] ifNotNil: [:value | self perform: sel with: value ]	
	 ]