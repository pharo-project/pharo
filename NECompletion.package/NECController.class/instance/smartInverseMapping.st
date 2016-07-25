settings
smartInverseMapping
	^ inverseMapping ifNil: [ 
		inverseMapping := Dictionary new.
		self smartCharactersMapping
			keysAndValuesDo: [ :key :value | inverseMapping at: value put: key ].
		inverseMapping ]