tests - multiply
testMultiplyByArray
	| newColor oldColor tolerance |
	tolerance := 0.001.

	oldColor := Color r: 0.75 g: 0.5 b: 0.25.
	newColor := oldColor * #(0.1 2 3).

	self assert: (0.075 - newColor red) abs < tolerance.
	self assert: (1 - newColor green) abs < tolerance.
	self assert: (0.75 - newColor blue) abs < tolerance.