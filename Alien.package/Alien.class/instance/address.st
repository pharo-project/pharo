accessing
address
	^self sizeField <= 0
		ifTrue: [self addressField]
		ifFalse: [self error: 'One cannot take the address of direct Alien instances; they may move']