private-formatting
formatMethodBodyFor: aMethodNode
	self
		indentAround:
			[ 
			self newLines: self newLinesAfterMethodPattern.
			self formatMethodCommentFor: aMethodNode.
			self formatPragmasFor: aMethodNode.
			self visitNode: aMethodNode body ]