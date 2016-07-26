*Polymorph-Widgets
restore
	"Restore the receiver's normal size."

	self isMinimized
		ifTrue: [self collapseBoxHit]
		ifFalse: [self isMaximized ifTrue: [self expandBoxHit]]