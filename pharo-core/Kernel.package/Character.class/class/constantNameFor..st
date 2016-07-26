private
constantNameFor: aCharacter
	^ self constantNames
		detect: [ :each | (self perform: each) = aCharacter ]
		ifNone: [ nil ].