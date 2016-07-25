private
verticalScrollBarValueToRowIndex: aNumber
	| startingIndex |
	startingIndex := self dataSource numberOfRows - self container calculateMinVisibleRows + 1.
	^ (startingIndex * aNumber) asInteger