converting
objectAsHexBytes
	^ String
		streamContents: [ :s | 
			s nextPutAll: '['.
			self object 
				do: [ :each | s << (each printPaddedWith: $0 to: 2 base: 16) ] 
				separatedBy: [ s nextPut: $  ].
			s nextPut: $] ]
		limitedTo: 5000