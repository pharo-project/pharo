menus
adjustedCenter: c
	"Set the receiver's position based on the #adjustedCenter protocol for adhereToEdge.  By default this simply sets the receiver's center.   Though there are (at its inception anyway) no other implementors of this method, it is required in use with the #adhereToEdge when the centering of a submorph is to be with reference to a rectangle  other than the receiver's center."

	self center: c