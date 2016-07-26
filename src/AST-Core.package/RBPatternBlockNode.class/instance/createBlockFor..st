matching
createBlockFor: aRBBlockNode 
	aRBBlockNode left: self left; right:self right.
	self replacePatternNodesIn: aRBBlockNode.
	^Smalltalk compiler 
		source: aRBBlockNode formattedCode;
		receiver: self;
		evaluate