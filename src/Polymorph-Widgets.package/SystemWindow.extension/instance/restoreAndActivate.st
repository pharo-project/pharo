*Polymorph-Widgets
restoreAndActivate
	"Restore the window if minimised then activate."

	self isMinimized
		ifTrue: [self restore].
	self isActive
		ifFalse: [self activate]