Extension { #name : #TaSingleComposition }

{ #category : #'*TraitsV2-Compatibility' }
TaSingleComposition >> includesElement: anElement [ 
	^ self == anElement or: [ inner == anElement ]
]

{ #category : #'*TraitsV2-Compatibility' }
TaSingleComposition >> subject [
	"Do not use... do not access directly to the subject. Kept for compatibility"
	^ inner
]

{ #category : #'*TraitsV2-Compatibility' }
TaSingleComposition >> transformations [
	^ { self }
]
