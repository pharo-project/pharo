evaluating
DoIt
	^ MCWorkingCopy
		managersForClass: MFClassA
		do: [:p | ^ p first] 