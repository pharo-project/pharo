Extension { #name : 'GoferResolvedReference' }

{ #category : '*Metacello-MC' }
GoferResolvedReference >> asMetacelloCachingResolvedReference [

	^MetacelloCachingGoferResolvedReference name: self name repository: self repository
]
