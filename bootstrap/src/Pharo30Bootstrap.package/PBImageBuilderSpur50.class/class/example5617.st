examples
example5617

	| vm om |
	vm := StackInterpreterSimulator newWithOptions: #(#ObjectMemory #Spur32BitMemoryManager ).
	om := vm objectMemory.
	vm desiredNumStackPages: 8. "Makes simulation faster by creating fewer stack pages."
	vm instVarNamed: 'assertVEPAES' put: false.
	vm initStackPages.
	PBImageBuilderSpur50 new
		version: '5.617';
		espellBackend: (EPSimulatorBackend simulator: vm)
			forBootstrap;
		bootstrap.