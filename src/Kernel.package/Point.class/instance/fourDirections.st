point functions
fourDirections
	"Return vertices for a square centered at 0 asPoint with the receiver as first corner.
	Returns the four rotation of the reciever in counter clockwise order with the receiver appearing last."
	
	^ Array with: self leftRotated
			with: self negated
			with: self rightRotated
			with: self 
		
