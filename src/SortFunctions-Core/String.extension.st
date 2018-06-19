Extension { #name : #String }

{ #category : #'*SortFunctions-Core' }
String >> threeWayCompareTo: aString [
	"Do a three-way comparison between the receiver and anotherObject, returning
	-1 if self < anotherObject
	0 if self = anotherObject
	1 if self > anotherObject
	This assumes a total order in accordance with the mathematical law of trichotomy.
	See also:  http://en.wikipedia.org/wiki/Three-way_comparison"

	 ^ (self compare: self with: aString collated: AsciiOrder) - 2
]
