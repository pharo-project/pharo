transforming
compileHash
	| method statement hash |
	method := RBParser parseMethod: 'hash
		"Answer an integer value that is related to the identity of the receiver."'.
	statement := nil.
	variables reversed do: [ :each |
		hash := RBMessageNode 
			receiver: (RBVariableNode named: each)
			selector: #hash.
		statement := statement isNil
			ifTrue: [ hash ]
			ifFalse: [
				RBMessageNode
					receiver: hash
					selector: #bitXor:
					arguments: (Array with: statement) ] ].
	method addNode: statement; addReturn.
	self theClass
		compile: method formattedCode
		classified: #(comparing)