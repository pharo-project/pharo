controls
beUnresizeable

	isResizeable := false.
	
	"If isResizable is overriden to always return true, then it would break the invariant"
	self isResizeable ifFalse: [ self removeGrips ]