layout-properties
listSpacing
	"Layout specific. This property describes how the heights for different rows in a table layout should be handled.
		#equal - all rows have the same height
		#none - all rows may have different heights
	"
	| props |
	props := self layoutProperties.
	^props ifNil:[#none] ifNotNil:[props listSpacing].