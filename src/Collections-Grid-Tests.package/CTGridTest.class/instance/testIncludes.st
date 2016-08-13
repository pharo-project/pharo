torevisit- tests - accessing
testIncludes
	self assert:
			((1 to: 4)
				allSatisfy: [:i | grid1 includes: i])
