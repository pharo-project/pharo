Extension { #name : #EllipseMorph }

{ #category : #'*Deprecated90' }
EllipseMorph >> closestPointTo: aPoint [
	self deprecated: 'No senders in default image, removed in issue #6282.' on: '2020-05-02' in: #Pharo9.  
	^self intersectionWithLineSegmentFromCenterTo: aPoint
]
