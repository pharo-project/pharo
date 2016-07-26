coloring
reverse: aRectangle fillColor: aMask	
	"Change all the bits in the receiver's area that intersects with aRectangle 
	according to the mask. Black does not necessarily turn to white, rather it 
	changes with respect to the rule and the bit in a corresponding mask 
	location. Bound to give a surprise."

	self fill: aRectangle rule: Form reverse fillColor: aMask