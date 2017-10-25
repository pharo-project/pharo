activation
activate
	
	[ [ self handleArgument: (self arguments 
			ifEmpty: [ '' ] 
			ifNotEmpty: [ :arguments| arguments first ])] 
		on: Exit 
		do: [ :exit | ^ self handleExit: exit ] ]
			forkAt: Processor userSchedulingPriority
			named: 'CommandLine handler process'