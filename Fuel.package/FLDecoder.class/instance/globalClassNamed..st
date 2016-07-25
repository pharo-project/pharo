accessing
globalClassNamed: className

	^ globalEnvironment 
		at: className
		ifAbsent: [ FLClassNotFound signalWithName: className ]