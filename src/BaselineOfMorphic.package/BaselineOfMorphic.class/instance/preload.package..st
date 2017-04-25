actions
preload: loader package: packageSpec

	'Starting preload action' logCr.
	
	initializersEnabled := MCMethodDefinition initializersEnabled.
	MCMethodDefinition initializersEnabled: false.
	
	Author fullName: self class name.
	
