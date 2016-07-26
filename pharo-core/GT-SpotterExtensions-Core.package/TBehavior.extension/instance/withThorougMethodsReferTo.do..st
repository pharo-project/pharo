*GT-SpotterExtensions-Core-private
withThorougMethodsReferTo: aLiteral do: aBlock
	| special byte |
	special := Smalltalk hasSpecialSelector: aLiteral ifTrueSetByte: [ :value | byte := value ].
	self selectorsAndMethodsDo: [ :selector :method |
		((method refersToLiteral: aLiteral) 
			or: [ special and: [ method scanFor: byte ] ]) 
				ifTrue: [ aBlock value: method ] ]