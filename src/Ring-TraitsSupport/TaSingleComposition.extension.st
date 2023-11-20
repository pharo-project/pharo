Extension { #name : 'TaSingleComposition' }

{ #category : '*Ring-TraitsSupport' }
TaSingleComposition >> includesElement: anElement [
	^ self == anElement or: [ inner == anElement ]
]
