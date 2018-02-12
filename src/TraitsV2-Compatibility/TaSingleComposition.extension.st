Extension { #name : #TaSingleComposition }

{ #category : #'*TraitsV2-Compatibility' }
TaSingleComposition >> includesElement: anElement [ 
	^ self == anElement or: [ inner == anElement ]
]

{ #category : #'*TraitsV2-Compatibility' }
TaSingleComposition >> subject [
	^ inner
]

{ #category : #'*TraitsV2-Compatibility' }
TaSingleComposition >> transformations [
	^ { self }
]
