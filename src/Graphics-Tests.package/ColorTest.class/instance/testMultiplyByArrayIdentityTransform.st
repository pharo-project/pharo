tests - multiply
testMultiplyByArrayIdentityTransform
	| newColor oldColor tolerance |
	tolerance := 0.001.

	oldColor := Color r: 0.75 g: 0.5 b: 0.25.
	newColor := oldColor * 2.

	self assert: (1 - newColor red) abs < tolerance.
	self assert: (1 - newColor green) abs < tolerance.
	self assert: (0.5 - newColor blue) abs < tolerance.