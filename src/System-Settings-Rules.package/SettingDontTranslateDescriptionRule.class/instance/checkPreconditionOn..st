helpers
checkPreconditionOn: aMethodNode

	^ aMethodNode pragmas anySatisfy: [ :p |
		p selector = #systemsettings ]