Extension { #name : 'TaSingleComposition' }

{ #category : '*Traits-Compatibility' }
TaSingleComposition >> includesElement: anElement [
	^ self == anElement or: [ inner == anElement ]
]

{ #category : '*Traits-Compatibility' }
TaSingleComposition >> subject [
	"Do not use... do not access directly to the subject. Kept for compatibility"
	^ inner
]

{ #category : '*Traits-Compatibility' }
TaSingleComposition >> transformations [
	^ { self }
]
