private
calculateStartIndexWhenShowing: visibleRows
	"Answer the first row to show when showing visibleRows rows. 
	 This works in case we are exceeding the available rows to show"

	| currentIndex startIndex |
	currentIndex := self owner showIndex.
	currentIndex + visibleRows - 1 > self owner numberOfRows ifTrue: [ 
		currentIndex := self owner numberOfRows - visibleRows + 2].
	startIndex := currentIndex max: 1.
	self owner basicMoveShowIndexTo: startIndex.
	^startIndex
	