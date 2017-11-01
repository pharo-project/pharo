Extension { #name : #GoferResolvedReference }

{ #category : #'*metacello-mc' }
GoferResolvedReference >> asMetacelloCachingResolvedReference [

	^MetacelloCachingGoferResolvedReference name: self name repository: self repository
]
