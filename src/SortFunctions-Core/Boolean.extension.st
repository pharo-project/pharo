Extension { #name : #Boolean }

{ #category : #'*SortFunctions-Core' }
Boolean >> threeWayCompareTo: anotherObject [
	^ self asBit threeWayCompareTo: anotherObject asBit
]
