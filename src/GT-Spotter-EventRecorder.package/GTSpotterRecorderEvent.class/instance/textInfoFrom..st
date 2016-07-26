accessing
textInfoFrom: aString
	| strings hashes sizes specials |
	aString isNil ifTrue: [ ^ nil ].
	aString size isZero ifTrue: [ ^ nil ].
	strings := aString substrings.
	hashes := strings collect: [ :each | self hashForText: each ] as: Array.
	sizes := strings collect: #size as: Array.
	specials := strings collect: [ :each | each beginsWith: '#' ] as: Array.
	^ Array with: hashes with: sizes with: specials 