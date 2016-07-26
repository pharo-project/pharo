path commands
reflectedCurveVia: cp2 to: aPoint
	"Add a reflected cubic bezier curve, starting from current path endpoint and ending at aPoint.
	The first control point is calculated as a reflection from the current point, if the last command was also a cubic bezier curve.
	Otherwise, the first control point is the current point. The second control point is cp2."
	
	self subclassResponsibility 	