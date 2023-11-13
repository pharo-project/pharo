Extension { #name : 'TaCompositionElement' }

{ #category : '*Traits-Compatibility' }
TaCompositionElement >> includesElement: anElement [

	^ self == anElement
]

{ #category : '*Traits-Compatibility' }
TaCompositionElement >> isTrait [
	^ true
]

{ #category : '*Traits-Compatibility' }
TaCompositionElement >> transformations [
	^ { self innerClass }
]
