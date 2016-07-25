*Polymorph-Widgets
collapseBoxHit
	"The user has clicked on the collapse box.
	Collapse or expand the receiver as appropriate."
	
	self isCollapsed
		ifTrue: [self playRestoreUpSound]
		ifFalse: [self playMinimizeSound].
	self collapseOrExpand