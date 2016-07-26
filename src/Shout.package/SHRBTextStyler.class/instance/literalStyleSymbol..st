formatting
literalStyleSymbol: aValue
	aValue isSymbol
		ifTrue: [ ^ #symbol ].
	aValue isString
		ifTrue: [ ^ #string ].
	aValue isCharacter
		ifTrue: [ ^ #character ].
	aValue isNumber
		ifTrue: [ ^ #number ].
	aValue == true
		ifTrue: [ ^ #true ].
	aValue == false
		ifTrue: [ ^ #false ].
	aValue isNil
		ifTrue: [ ^ #nil ].
	^ #default