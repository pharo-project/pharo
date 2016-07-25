accessing
ensuredMap

	ensuredMap == nil ifTrue: [ ensuredMap := Dictionary new ].
	^ensuredMap