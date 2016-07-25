updating
refresh
	"Refreshes all internal values (forces an invalidate of all subviews)"
	self ensureAtLeastOneColumn.
	self recalculateVerticalScrollBar.
	self verticalScrollBar changed.
	self container changed
	