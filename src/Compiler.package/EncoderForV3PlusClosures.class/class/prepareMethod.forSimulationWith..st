compiled method support
prepareMethod: compiledMethod forSimulationWith: numArgs
	| xpc |
	xpc := compiledMethod initialPC.
	"long store temp"
	(compiledMethod at: xpc) = 129 
		ifTrue: [
			compiledMethod at: xpc + 1 put: (16r40 + numArgs).
			compiledMethod at: xpc + 3 put: (16r10 + numArgs)]