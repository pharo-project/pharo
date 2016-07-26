converting
asTextIndex
	"Answer the absolute index which corresponds to receiver's location inside text.
	The resulting number is equivalent to the number of times of moving text starting position
	right, to reach same position as the receiver.

	Please note that converting text index<->position does not takes constant time, 
	because it requires linear scan from the beginning of text.

	Also note that answered number does not corresponds to some the character index in text, 
	but to index of unique insertion point in text, with the exception that endpoints of adjacent spans
	considered as same insertion point, not two separate ones.
	
	This method is symmetrical to TxModel>>#positionAtTextIndex:. 
	"
	
	^ self text startPosition timesToMoveRightTo: self