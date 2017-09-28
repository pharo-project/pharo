initialization
initialize
	super initialize.
	builderAccessor := [ :e | e layoutDefinition sharedPools asArray ].
	classAccessor := [ :e | (e sharedPools collect:[:x | x name]) asArray ]