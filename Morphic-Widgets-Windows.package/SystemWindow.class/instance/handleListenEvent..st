events
handleListenEvent: evt
	"Make sure we lock our contents after DnD has finished"
	evt isMouse ifFalse:[^self].
	evt hand hasSubmorphs ifTrue:[^self]. "still dragging"
	self isTopWindow ifFalse:[self lockInactivePortions].
	evt hand removeMouseListener: self.