code generation (closures)
optimizedBlockHoistTempsInto: scopeBlock "<BlockNode>"
	"This is a No-op for all nodes except non-optimized BlockNodes."
	"Let's assume the special > 0 guard in MessageNode>>analyseTempsWithin:forValue:encoder: is correct.
	 Then we can simply hoist our temps up."
	[arguments isNil or: [arguments size <= 1]] assert.
	(arguments notNil and: [arguments notEmpty]) ifTrue:
		[scopeBlock addHoistedTemps: arguments.
		arguments := #()].
	temporaries notEmpty ifTrue:
		[scopeBlock addHoistedTemps: temporaries.
		temporaries := #()]