running
execute: aWorkingCopy
	aWorkingCopy packageSet methods
		do: [ :each | each actualClass recompile: each selector ]