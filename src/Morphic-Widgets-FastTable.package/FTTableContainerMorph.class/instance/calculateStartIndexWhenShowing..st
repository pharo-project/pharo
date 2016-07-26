private
calculateStartIndexWhenShowing: visibleRows
	"Answer the first row to show when showing visibleRows rows. 
	 This works in case we are exceeding the available rows to show"

	^ self owner showIndex max: 1