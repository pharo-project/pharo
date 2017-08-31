*GT-SpotterExtensions-Core-private
withMethodsReferTo: aLiteral do: aBlock
	| specialIndex |
	specialIndex := Smalltalk specialSelectorIndexOrNil: aLiteral.
	self selectorsAndMethodsDo: [ :selector :method | 
		((method hasLiteral: aLiteral) 
			or: [ specialIndex notNil and: [ method scanFor: method encoderClass firstSpecialSelectorByte + specialIndex ] ])
				ifTrue: [ aBlock value: method ] ]