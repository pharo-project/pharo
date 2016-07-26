path commands
curveVia: cp1 and: cp2 to: aPoint
	"Add a cubic bezier curve,
	starting from current path endpoint, using control points cp1, cp2
	and ending at aPoint "
	
	self subclassResponsibility 	