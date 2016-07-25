*Spec-MorphicAdapters
detectMorphicAdapterDo: aBlock

	^ self dependents 
		detect: [:e | e isMorphicAdapter ]
		ifFound: aBlock