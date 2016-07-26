generate
genForRBMethodNode
	^RBMessageNode 
		receiver: (RBVariableNode named: 'thisContext')
		selector: #receiver 
	