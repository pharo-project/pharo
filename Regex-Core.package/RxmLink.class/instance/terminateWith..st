building
terminateWith: aTerminator
	"Propagate this message along the chain of links, and
	make aTerminator the `next' link of the last link in the chain.
	If the chain is already reminated with the same terminator, 
	do not blow up."

	next == nil
		ifTrue: [next := aTerminator]
		ifFalse: [next terminateWith: aTerminator]