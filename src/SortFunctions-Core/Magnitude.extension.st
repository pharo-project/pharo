Extension { #name : #Magnitude }

{ #category : #'*SortFunctions-Core-comparing' }
Magnitude >> threeWayCompareTo: anotherObject [
	"Do a three-way comparison between the receiver and anotherObject, returning
	-1 if self < anotherObject
	0 if self = anotherObject
	1 if self > anotherObject
	This assumes a total order in accordance with the mathematical law of trichotomy.
	See also:  http://en.wikipedia.org/wiki/Three-way_comparison"

	^ self = anotherObject
		ifTrue: [ 0 ]
		ifFalse: [ self < anotherObject
				ifTrue: [ -1 ]
				ifFalse: [ 1 ] ]
]
