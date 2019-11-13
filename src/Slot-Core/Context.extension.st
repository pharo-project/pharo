Extension { #name : #Context }

{ #category : #'*Slot-Core' }
Context >> hasTemporaryVariableNamed: aName [
	^ self tempNames includes: aName
]
