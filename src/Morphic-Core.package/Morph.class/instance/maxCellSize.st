layout-properties
maxCellSize
	"Layout specific. This property specifies the maximum size of a table cell."
	| props |
	props := self layoutProperties.
	^props ifNil:[SmallInteger maxVal] ifNotNil:[props maxCellSize].