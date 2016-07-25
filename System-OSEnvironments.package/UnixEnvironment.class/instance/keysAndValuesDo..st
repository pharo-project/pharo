enumeration
keysAndValuesDo: aBlock
	| index associationString  |
	index := 1	.
	[ 
		associationString := self environAt: index.
		associationString ifNil: [ ^ self ].
		self keysAndValuesDo: aBlock withAssociationString: associationString.
		index := index + 1
	] repeat.