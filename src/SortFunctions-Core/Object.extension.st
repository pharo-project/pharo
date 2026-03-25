Extension { #name : 'Object' }

{ #category : '*SortFunctions-Core' }
Object >> threeWayCompareTo: anObject [
	"Do a three-way comparison between the receiver and anotherObject, returning
	-1 if self < anotherObject
	0 if self = anotherObject
	1 if self > anotherObject
	This assumes a total order in accordance with the mathematical law of trichotomy.
	See also:  http://en.wikipedia.org/wiki/Three-way_comparison.
	
	This default is a heuristic to be able to handle any kind of objects. 
	Each domain may specialize it. "

	^ self displayString threeWayCompareTo: anObject displayString
]
