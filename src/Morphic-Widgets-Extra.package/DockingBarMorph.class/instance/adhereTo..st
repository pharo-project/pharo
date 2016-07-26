private - accessing
adhereTo: edgeSymbol 
	"Private - Instruct the receiver to adhere to the given edge.  
	 
	Options: #left #top #right #bottom or #none"
	(#(#left #top #right #bottom #none ) includes: edgeSymbol)
		ifFalse: [^ self error: 'invalid option'].
	self setToAdhereToEdge: edgeSymbol.
	self updateLayoutProperties.
	self updateColor