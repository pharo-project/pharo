building actions
actionsForPragmas: aSymbolsCollection

	self session interruptedContext ifNil: [ ^ #() ].
	^ (self allActionsForPragmas: aSymbolsCollection)
				collect: [ :aDebugAction | aDebugAction asGlamourAction ]
