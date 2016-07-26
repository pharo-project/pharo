*Polymorph-Widgets
removeLabelArea
	"Remove the entire label area."

	self removeGrips.
	labelArea delete.
	label := nil.	
	(self isCollapsed not or: [self isTaskbarPresent]) ifTrue: [
		self addGripsIfWanted]