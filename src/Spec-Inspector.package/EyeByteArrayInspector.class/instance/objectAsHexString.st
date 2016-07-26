converting
objectAsHexString
	^ String
		streamContents: [ :s | 
			self object do: [ :each | s << (each printPaddedWith: $0 to: 2 base: 16) ] ]
		limitedTo: 5000