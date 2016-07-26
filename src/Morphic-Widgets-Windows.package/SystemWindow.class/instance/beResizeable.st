controls
beResizeable

	isResizeable := true.
	
	"If isResizable is overriden to always return false, then it would break the invariant"
	self isResizeable ifTrue: [ self addGripsIfWanted ]