private
moveShowIndexTo: aNumber
	"I move the showing index to a specific row, and perform a refresh of subviews. 
	 I should not be used directly, and most methods that need to move the 
	 showing pointer should do it directly. 
	 Use me just in case you need to force a refresh after settign the index"
	self basicMoveShowIndexTo: aNumber.
	self verticalScrollBar value: (self rowIndexToVerticalScrollBarValue: aNumber).
	self refresh