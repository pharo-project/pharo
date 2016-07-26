initialization
initializeColumns
	"you can initialize only if entity and table both not nil"
	self removeAllBricks.
	(self entity isNil | self table isNil) ifTrue: [ ^ self ].
	self table columns do: [ :each | self addColumn: each ].
	self updateWidthPercentage