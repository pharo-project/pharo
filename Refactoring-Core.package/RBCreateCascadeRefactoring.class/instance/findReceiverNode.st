preconditions
findReceiverNode
	"Find the sequence to be combined."

	| receiverNodes |
	receiverNodes := statementNodes 
		collect: [ :each | each receiver ].
	receiverNodes asSet size = 1
		ifFalse: [ self refactoringError: 'All statements must have the same receiver' ].
	(receiverNodes first isLiteralNode or: [ receiverNodes first isVariable ])
		ifFalse: [ self refactoringWarning: 'The receiver is an expression. Proceed with caution' ]