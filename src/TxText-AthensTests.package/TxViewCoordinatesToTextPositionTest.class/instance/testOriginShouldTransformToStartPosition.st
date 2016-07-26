tests
testOriginShouldTransformToStartPosition
	| position |
	position := view textPositionAt: 0@0.
	self assert: position asTextIndex = view layout startPosition asTextIndex .