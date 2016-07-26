menus
adhereToEdge: edgeSymbol 
	| edgeMessage |
	(owner isNil or: [owner isHandMorph]) ifTrue: [^self].
	(owner class canUnderstand:  edgeSymbol) ifFalse:  [^self].
	(self class canUnderstand: ( edgeMessage := (edgeSymbol , ':') asSymbol ))
		 ifFalse:  [^self].
	
	self perform: edgeMessage
		withArguments: (Array with: (owner perform: edgeSymbol))