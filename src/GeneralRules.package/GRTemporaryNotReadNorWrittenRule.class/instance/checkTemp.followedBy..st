running
checkTemp: aTemp followedBy: statements
	| isRead isWritten |
	isRead := isWritten := false.
	statements do: [ :statement | 
		statement nodesDo: [ :node | 
			(node isVariable and: [
			 node name = aTemp name ]) ifTrue: [
					isRead    := isRead    or: [ node isUsed ].
					isWritten := isWritten or: [ node isWrite ] ] ] ].
		
	^ (isRead and: isWritten )