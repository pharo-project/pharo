drawing
text
	| keyParts |
	keyParts := (self keyText substrings: '+') collect: #trimBoth.

	"Ensure shift is first"
	((keyParts includes: 'Shift') and: [ (keyParts first = 'Shift') not ])
		ifTrue: [ keyParts := (keyParts copyWithout: 'Shift') copyWithFirst: 'Shift' ].
	^ keyParts inject: Text new into: [ :s :each | s , (self class symbolTableAt: each ifAbsent: [ each capitalized ]) ]