accessing
intermediateIndices

	| originRowIndex targetRowIndex |
	originRowIndex := origin rowIndex.
	targetRowIndex := target rowIndex.

	^ originRowIndex >= targetRowIndex
		ifTrue: [ originRowIndex - 1 to: targetRowIndex + 1 by: -1 ]
		ifFalse: [ originRowIndex + 1 to: targetRowIndex - 1 ]