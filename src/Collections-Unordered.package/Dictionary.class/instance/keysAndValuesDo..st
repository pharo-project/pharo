enumerating
keysAndValuesDo: aBlock
	^self associationsDo:[:assoc|
		aBlock value: assoc key value: assoc value].