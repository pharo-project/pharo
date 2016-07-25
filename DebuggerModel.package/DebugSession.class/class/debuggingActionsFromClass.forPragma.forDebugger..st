actions registration
debuggingActionsFromClass: aClass forPragma: aSymbol forDebugger: aDebugger
	| pragmas actions |
	pragmas := Pragma
		allNamed: aSymbol
		from: aClass class
		to: aClass class.
	actions := OrderedCollection new.
	pragmas
		do: [ :aPragma | 
			actions
				addAll:
					((aPragma methodClass soleInstance
						perform: aPragma methodSelector
						withEnoughArguments: {aDebugger}) asOrderedCollection
						collect: [ :each | each asDebugAction ]) ].
	^ actions