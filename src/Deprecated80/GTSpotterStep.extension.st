Extension { #name : #GTSpotterStep }

{ #category : #'*Deprecated80' }
GTSpotterStep >> isEmpty [
	self deprecated: 'Use hasCandidates instead'.
	^ self hasCandidates not
]
