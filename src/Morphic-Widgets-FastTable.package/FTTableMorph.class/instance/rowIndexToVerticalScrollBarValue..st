private
rowIndexToVerticalScrollBarValue: aNumber
	| numberOfRows |
	numberOfRows := self dataSource numberOfRows - self container calculateMinVisibleRows.
	^ (numberOfRows = 0 or: [ aNumber <= 1 ])
		ifTrue: [ 0.0 ]
		ifFalse: [ ((aNumber / numberOfRows) asFloat max: 0.0) min: 1.0 ]