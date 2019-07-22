Extension { #name : #SpMorphicCodeAdapter }

{ #category : #'*Spec2-Deprecated' }
SpMorphicCodeAdapter >> isAboutToStyle [
	self deprecated: 'Use #okToStle instead' transformWith: '`@receiver isAboutToStyle' -> '`@receiver okToStyle'.
	^ self okToStyle
]
