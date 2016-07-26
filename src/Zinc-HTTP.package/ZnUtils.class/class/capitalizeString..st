conversions
capitalizeString: string
	| capitalExpected |
	capitalExpected := true.
	^ String new: string size streamContents: [ :stream |
		string do: [ :each |
			each isAlphaNumeric
				ifTrue: [
					capitalExpected 
						ifTrue: [ 
							stream nextPut: each asUppercase.
							capitalExpected := false. ]
						ifFalse: [ 
							stream nextPut: each asLowercase ] ] 
		 		ifFalse: [ 
					stream nextPut: each.
					capitalExpected := true ] ] ]