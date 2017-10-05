initialization
initialize
	super initialize.
	builderAccessor := [ :e | e superclass ].
	classAccessor := [ :e | e superclass ]