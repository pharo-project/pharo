as yet unclassified
DoIt
	^ MCWorkingCopy
		managersForClass: MFClassA
		do: [:p | ^ p first] 