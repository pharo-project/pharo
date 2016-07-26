accessing
node: aVariableNode

	super node: aVariableNode.
	messageText := 'Undeclared temp: ', aVariableNode name.
