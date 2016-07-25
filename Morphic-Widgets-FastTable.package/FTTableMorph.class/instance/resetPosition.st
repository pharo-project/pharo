private
resetPosition
	"Resets all values to original value"
	showIndex := 0.
	self verticalScrollBar value: 0.
	self container setNeedsRefreshExposedRows.
	self container updateExposedRows.