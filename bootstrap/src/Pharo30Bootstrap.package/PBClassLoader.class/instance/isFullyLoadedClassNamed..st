initialization
isFullyLoadedClassNamed: aClassName

	^ fullyLoaded at: aClassName ifAbsent: [ false ]