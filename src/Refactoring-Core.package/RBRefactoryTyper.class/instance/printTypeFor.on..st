printing
printTypeFor: aString on: aStream 
	| types |
	types := (self guessTypesFor: aString) 
				asSortedCollection: [:a :b | a name < b name].
	1 to: types size
		do: 
			[:i | 
			i == 1 ifFalse: [aStream nextPutAll: ' | '].
			self 
				printType: (types at: i)
				for: aString
				on: aStream]