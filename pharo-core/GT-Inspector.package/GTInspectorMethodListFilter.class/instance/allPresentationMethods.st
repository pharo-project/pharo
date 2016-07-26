accessing
allPresentationMethods

	^ (PragmaCollector allSystemPragmas select: [ :pragma | 
		pragma key == self inspectorPragmaKey ]) collect: [ :aPragma | aPragma method ]