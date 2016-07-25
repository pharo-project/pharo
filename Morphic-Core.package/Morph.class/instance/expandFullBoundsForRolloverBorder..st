drawing
expandFullBoundsForRolloverBorder: aRectangle
	| delta |
	delta := self valueOfProperty: #rolloverWidth ifAbsent: [10@10].
	^aRectangle expandBy: delta.

