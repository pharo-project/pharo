initialization
initialize
	super initialize.
	builderAccessor := [ :e | e layoutDefinition slots ].
	classAccessor := [ :e | e slots ]