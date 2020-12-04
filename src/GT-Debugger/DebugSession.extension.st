Extension { #name : #DebugSession }

{ #category : #'*GT-Debugger' }
DebugSession class >> debuggingActionsForPragma: aSymbol for: aDebugger [
	^ (DebugAction allSubclasses reject: [ :each | each hasAbstractMethods ])
		inject: OrderedCollection new
		into: [ :currentActions :aClass | 
			currentActions
				addAll: (self debuggingActionsFromClass: aClass forPragma: aSymbol forDebugger: aDebugger);
				yourself ]
]

{ #category : #'*GT-Debugger' }
DebugSession class >> debuggingActionsForPragmas: aSymbolsCollection for: aDebugger [
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
]

{ #category : #'*GT-Debugger' }
DebugSession class >> debuggingActionsFromClass: aClass forPragma: aSymbol forDebugger: aDebugger [
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
]

{ #category : #'*gt-debugger' }
DebugSession >> isActive: aContext [

	^ true
]
