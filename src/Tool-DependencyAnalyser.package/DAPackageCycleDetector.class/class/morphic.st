instance-creation
morphic
	^ (self onPackagesNamed: #('Morphic-Base' 'Morphic-Core' 'Morphic-Examples' 'MorphicTests' 'Spec-MorphicAdapters')) runAlgorithm.