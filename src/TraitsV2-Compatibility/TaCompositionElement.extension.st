Extension { #name : #TaCompositionElement }

{ #category : #'*TraitsV2-Compatibility' }
TaCompositionElement >> includesElement: anElement [
 
	^ self == anElement
]

{ #category : #'*TraitsV2-Compatibility' }
TaCompositionElement >> isTrait [
	^ true
]

{ #category : #'*TraitsV2-Compatibility' }
TaCompositionElement >> transformations [
	^ { self innerClass }
]
