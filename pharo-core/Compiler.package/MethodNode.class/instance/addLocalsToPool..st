code generation (closures)
addLocalsToPool: locals "<Set of: TempVariableNode>"
	localsPool ifNil:
		[localsPool := IdentitySet new].
	localsPool addAll: locals