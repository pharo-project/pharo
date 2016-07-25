actions
recompilePackage
	workingCopy package packageSet methods
		do: [:ea | ea actualClass recompile: ea selector]
		displayingProgress: 'Recompiling...'