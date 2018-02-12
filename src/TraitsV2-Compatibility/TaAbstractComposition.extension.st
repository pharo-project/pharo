Extension { #name : #TaAbstractComposition }

{ #category : #'*TraitsV2-Compatibility' }
TaAbstractComposition >> includesElement: anElement [
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
	self subclassResponsibility
]
