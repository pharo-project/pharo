actions registration
debuggingActionsForPragmas: aSymbolsCollection for: aDebugger
	self flag: 'split me'.
	^ (((aSymbolsCollection 
		inject: OrderedCollection new
		into: [ :currentActions :aSymbol | 
			currentActions 
				addAll: (self debuggingActionsForPragma: aSymbol for: aDebugger);
				yourself ]) 
					select: [ :aDebugAction | 
						aDebugAction appliesToDebugger: aDebugger ]) 
					collect: [ :aDebugAction | 
						aDebugAction
							forDebugger: aDebugger;
							yourself ]) 
					sort: [ :action1 :action2 | 
						action1 order < action2 order ]