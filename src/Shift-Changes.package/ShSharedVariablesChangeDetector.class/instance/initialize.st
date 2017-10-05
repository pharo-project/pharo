initialization
initialize
	super initialize.
	builderAccessor := [ :e | e layoutDefinition sharedVariables collect:[ :x | x key -> x class ] as: Array ].
	classAccessor := [ :e | e classVariables collect: [ :x | x key -> x class ] as: Array ]