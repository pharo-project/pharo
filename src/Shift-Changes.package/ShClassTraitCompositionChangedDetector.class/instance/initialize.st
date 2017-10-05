initialization
initialize
	super initialize.
	builderAccessor := [ :e | self enhancer classTraitCompositionOf: e ].
	classAccessor := [ :e | e class traitComposition ]