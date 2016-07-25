analyzing
referencesOf: anObject do: aBlock
	aBlock value: anObject size.
	anObject
		keysAndValuesDo: [ :key :value | 
			aBlock value: key. 
			aBlock value: value.  ]
