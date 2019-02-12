Extension { #name : #GTSpotterCandidatesProcessor }

{ #category : #'*GT-Spotter-UI' }
GTSpotterCandidatesProcessor >> render: anObject [
	^ GTSpotterItemBrick new
		icon: (self itemIconFor: anObject);
		text: (self itemNameFor: anObject)
]
