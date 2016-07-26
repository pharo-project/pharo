layout-properties
minCellSize
	"Layout specific. This property specifies the minimal size of a table cell."
	| props |
	props := self layoutProperties.
	^props ifNil:[0] ifNotNil:[props minCellSize].