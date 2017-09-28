initialization
initialize
	super initialize.
	builderAccessor := [ :e | e layoutDefinition sharedVariablesString ].
	classAccessor := [ :e | e classVariablesString ]