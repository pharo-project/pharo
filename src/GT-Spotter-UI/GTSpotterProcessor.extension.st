Extension { #name : #GTSpotterProcessor }

{ #category : #'*GT-Spotter-UI' }
GTSpotterProcessor >> render: anObject [
	^ GTSpotterItemBrick new
		icon: (nil);
		text: (anObject gtDisplayString)
]
