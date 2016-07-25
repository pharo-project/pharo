converting
asUncommentedCode
	"this string reprensent a commented code, let's uncomment it"
	
	^String streamContents:  [ :str |
		
		|doubleQuoteAlreadyFound|
		doubleQuoteAlreadyFound := false.
		
		self withoutQuoting do: [ :char |
			char = $"
				ifTrue: [ 
					doubleQuoteAlreadyFound 	ifTrue: [ 
							str nextPut: $" ].
					doubleQuoteAlreadyFound := doubleQuoteAlreadyFound not.
				]
				ifFalse: [ 
					str nextPut: char
				]
		 ]
	]

