actions registration
debuggingActionsForPragma: aSymbol for: aDebugger
	^ (DebugAction allSubclasses reject: [ :each | each hasAbstractMethods ])
		inject: OrderedCollection new
		into: [ :currentActions :aClass | 
			currentActions
				addAll: (self debuggingActionsFromClass: aClass forPragma: aSymbol forDebugger: aDebugger);
				yourself ]