layout
minHeight
	"Fit the width of the scroll target if vResizing is shrinkWrap."
	
	^self vResizing = #shrinkWrap
		ifTrue: [self scrollTarget minExtent y + self scrollbarThickness + 5]
		ifFalse: [super minHeight]