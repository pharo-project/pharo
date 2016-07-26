testing
isCapitalizedString: string
	| capitalExpected |
	capitalExpected := true.
	string do: [ :each |
		each isAlphaNumeric
			ifTrue: [
				((capitalExpected and: [ each isUppercase ])
					or: [ capitalExpected not and: [ each isLowercase ] ])
					ifTrue: [ capitalExpected := false ]
					ifFalse: [ ^ false ] ] 
		 	ifFalse: [ capitalExpected := true ] ].
	^ true



