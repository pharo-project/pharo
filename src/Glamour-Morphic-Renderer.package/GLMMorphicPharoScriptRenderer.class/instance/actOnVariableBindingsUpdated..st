callbacks
actOnVariableBindingsUpdated: ann 

	textModel variableBindings: nil.
	textModel appendVariableBinding: ann presentation variableBindings.
	textMorph shoutStyler style: textMorph text.