*Spec-MorphicAdapters
detectMorphicAdapterDo: doBlock ifNone: aBlock

	^ self dependents 
		detect: [:e | e isMorphicAdapter ]
		ifFound: doBlock
		ifNone: aBlock