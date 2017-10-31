*Spec-MorphicAdapters
ensureMorphicAdapter

	^ self dependents 
		detect: [:e | e isMorphicAdapter ]
		ifNone: [ self buildWithSpec ]