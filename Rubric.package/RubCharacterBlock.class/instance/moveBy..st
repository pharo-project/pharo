private
moveBy: aPoint 
	"Change the corner positions of the receiver so that its area translates by 
	the amount defined by the argument, aPoint."
	surface := surface translateBy: aPoint