Extension { #name : #TaAbstractComposition }

{ #category : #'*TraitsV2-Compatibility' }
TaAbstractComposition >> includesElement: anElement [
	"It checks if the composition includes the given element in the trait composition. It is used for compatibility, it should not be used as it is comparing by identity."
	^ self subclassResponsibility 
]

{ #category : #'*TraitsV2-Compatibility' }
TaAbstractComposition >> isTraitAlias [
	^ false
]

{ #category : #'*TraitsV2-Compatibility' }
TaAbstractComposition >> isTraitExclusion [
	^ false
]

{ #category : #'*TraitsV2-Compatibility' }
TaAbstractComposition >> transformations [
	"For compatibility with old code, does not use! 
	It returns all the elements in the trait composition"
	self subclassResponsibility
]
