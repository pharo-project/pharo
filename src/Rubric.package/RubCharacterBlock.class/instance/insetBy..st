private
insetBy: aPoint 
	"Change the corner positions of the receiver so that its area translates by 
	the amount defined by the argument, aPoint."
	^ self copy surface: (surface insetBy: aPoint)