examples
grid22
	| grid |
	grid := self new: 2.
	grid atRow: 1 atColumn: 1 put: 1.
	grid atRow: 1 atColumn: 2 put: 3.
	grid atRow: 2 atColumn: 1 put: 2.
	grid atRow: 2 atColumn: 2 put: 4.
	^ grid