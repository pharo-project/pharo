private
isNonEmptySingleLineBlock: aBlockNode
	^ ((self willBeMultiline: aBlockNode body) or: [ aBlockNode body statements isEmpty ]) not