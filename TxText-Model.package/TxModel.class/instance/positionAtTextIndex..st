positioning
positionAtTextIndex: textIndex
	"Answer the position, which corresponds to given text index. This method is symmetrical to TxBasicTextPosition>>#textIndex. 
	
	Please note that converting text index<->position does not take constant time, because it requires linear scan from the beginning of text.

	Also note that text index does not correspond to some character index in the text, but to the index of a unique insertion point in the text, with the exception that endpoints of adjacent spans are considered as the same (single) insertion point, not two separate ones."

	^ self startPosition moveRight: textIndex