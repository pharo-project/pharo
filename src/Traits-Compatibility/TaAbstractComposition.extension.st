Extension { #name : 'TaAbstractComposition' }

{ #category : '*Traits-Compatibility' }
TaAbstractComposition >> includesElement: anElement [
	"It checks if the composition includes the given element in the trait composition. It is used for compatibility, it should not be used as it is comparing by identity."
	^ self subclassResponsibility
]

{ #category : '*Traits-Compatibility' }
TaAbstractComposition >> isTraitAlias [
	^ false
]

{ #category : '*Traits-Compatibility' }
TaAbstractComposition >> isTraitExclusion [
	^ false
]

{ #category : '*Traits-Compatibility' }
TaAbstractComposition >> transformations [
	"For compatibility with old code, does not use!
	It returns all the elements in the trait composition"
	self subclassResponsibility
]
