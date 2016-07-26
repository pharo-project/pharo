*GT-SpotterExtensions-Core-private
withMethodSender: aLiteral do: aBlock
	| special byte |
	special := Smalltalk hasSpecialSelector: aLiteral ifTrueSetByte: [ :value | byte := value ].
	self selectorsAndMethodsDo: [ :selector :method | 
		((method hasLiteral: aLiteral) 
			or: [ (method refersToLiteral: aLiteral) 
				or: [ special and: [ method scanFor: byte ] ] ])
					ifTrue: [ aBlock value: method ] ].