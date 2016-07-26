private-formatting
formatMethodBodyFor: aMethodNode
	self
		indentAround:
			[ 
			self newLine.
			self formatMethodCommentFor: aMethodNode.
			self formatPragmasFor: aMethodNode.
			self visitNode: aMethodNode body ]