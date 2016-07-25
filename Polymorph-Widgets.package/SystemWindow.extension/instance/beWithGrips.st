*Polymorph-Widgets
beWithGrips
	"Add the grips and set a property to
	indicate that grips are wanted."

	self removeProperty: #noGrips.
	(self isCollapsed not or: [self isTaskbarPresent]) ifTrue: [
		self addGripsIfWanted]