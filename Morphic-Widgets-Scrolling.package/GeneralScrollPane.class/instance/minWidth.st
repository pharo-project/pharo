layout
minWidth
	"Fit the width of the scroll target if hResizing is shrinkWrap."
	
	^self hResizing = #shrinkWrap
		ifTrue: [self scrollTarget minExtent x + self scrollbarThickness + 5]
		ifFalse: [super minWidth]