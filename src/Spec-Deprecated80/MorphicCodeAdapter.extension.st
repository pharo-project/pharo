Extension { #name : #MorphicCodeAdapter }

{ #category : #'*Spec-Deprecated80' }
MorphicCodeAdapter >> isAboutToStyle [
	self deprecated: 'Use #okToStle instead' transformWith: '`@receiver isAboutToStyle' -> '`@receiver okToStyle'.
	^ self okToStyle
]
