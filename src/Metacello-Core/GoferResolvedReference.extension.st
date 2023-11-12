Extension { #name : 'GoferResolvedReference' }

{ #category : '*Metacello-Core' }
GoferResolvedReference >> asMetacelloCachingResolvedReference [

	^MetacelloCachingGoferResolvedReference name: self name repository: self repository
]
