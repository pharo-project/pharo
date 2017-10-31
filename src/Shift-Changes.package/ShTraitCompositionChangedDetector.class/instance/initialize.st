initialization
initialize
	super initialize.
	builderAccessor := [ :e | self enhancer traitCompositionOf: e ].
	classAccessor := [ :e | e traitComposition ].
	comparer := [ :a :b | a asString = b asString ]