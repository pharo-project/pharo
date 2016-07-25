visitor-double dispatching
visitMessageNode: aMessageNode
	aMessageNode isInlined ifTrue: [
		methodBuilder addLiteral: aMessageNode selector. "so searching for senders will work"
		^self 
			perform: (OptimizedMessages at: aMessageNode selector)
			with: aMessageNode].
	^ self emitMessageNode: aMessageNode