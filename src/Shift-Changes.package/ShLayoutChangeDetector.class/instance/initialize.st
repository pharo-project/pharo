initialization
initialize
	super initialize.
	builderAccessor := [ :e | e layoutDefinition layoutClass ].
	classAccessor := [ :e | e classLayout class name ]