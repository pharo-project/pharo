matching
constructLookupNodeFor: aString in: aRBBlockNode 
	| argumentNode |
	argumentNode := RBLiteralNode value: aString.
	
	^RBMessageNode 
		receiver: (RBVariableNode named: 'self')
		selector: #lookupMatchFor:in:
		arguments: (Array with: argumentNode with: aRBBlockNode arguments last)