private
targetClass
	[ ^ self classToBeTested ]
		on: Error
		do: [ 
			| className |
			className := self class name asString copyFrom: 1 to: self class name size - 4.
			^ Smalltalk globals at: className asString asSymbol ]