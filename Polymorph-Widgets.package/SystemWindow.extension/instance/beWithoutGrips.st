*Polymorph-Widgets
beWithoutGrips
	"Remove the grips and set a property to
	indicate that grips are not wanted."

	self setProperty: #noGrips toValue: true.
	self removeGrips