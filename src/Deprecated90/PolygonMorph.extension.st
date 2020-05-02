Extension { #name : #PolygonMorph }

{ #category : #'*Deprecated90' }
PolygonMorph >> closestPointTo: aPoint [ 
	| closestPoint minDist |
	self deprecated: 'No senders in default image, removed in issue #6282.' on: '2020-05-02' in: #Pharo9.  
	closestPoint := minDist := nil.
	self lineSegmentsDo: 
			[:p1 :p2 | | dist curvePoint | 
			curvePoint := aPoint nearestPointOnLineFrom: p1 to: p2.
			dist := curvePoint distanceTo: aPoint.
			(closestPoint isNil or: [dist < minDist]) 
				ifTrue: 
					[closestPoint := curvePoint.
					minDist := dist]].
	^closestPoint
]
