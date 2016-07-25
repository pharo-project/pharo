analyzing
referencesOf: anObject do: aBlock
	aBlock value: anObject size.
	anObject
		do: [ :each | aBlock value: each ]
