drawing-support
translateBy: delta during: aBlock
	self apply: [ :clippedCanvas |
		 clippedCanvas translateBy: delta during: aBlock ]