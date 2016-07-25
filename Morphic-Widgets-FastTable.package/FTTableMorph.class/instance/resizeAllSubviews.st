private
resizeAllSubviews
	self resizeVerticalScrollBar.
	"if we resized scrollbar, we need to recalculate it because values change (and now visibility 
	 can be toggled, shown items can change, etc.)"
	self recalculateVerticalScrollBar.
	self resizeContainer.
	self resizeColumns.
	function isExplicite
		ifTrue: [ function resizeWidget ]