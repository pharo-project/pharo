Extension { #name : #CmCommand }

{ #category : #'*NewTools-Core' }
CmCommand >> appliesTo: aTool [

	^ true
]

{ #category : #'*NewTools-Core' }
CmCommand class >> commandClassesFromPragma: aSymbol [
	"Collects command classes matching the pragma aSymbol. 
	 WARNING: The collection will happen takign the caller class and subclasses. If you 
	 execute: CmCommand commandClassesForPragma: #myPragma:, it will scan all commands in 
	 system. THIS IS NOT RECOMMENDED. Please use your own hierarchy as starting point"
	| commandsClasses |
	
	commandsClasses := OrderedCollection new.
	(self allSubclasses reject: [ :sc | sc hasAbstractMethods ])
		do: [ :commandClass | 
			commandsClasses addAll: (Pragma
				allNamed: aSymbol
				from: commandClass class
				to: commandClass class) ].
	
	^ ((aSymbol last = $:)
		ifTrue: [ commandsClasses sorted: [ :a :b | (a argumentAt: 1) < (b argumentAt: 1) ] ]
		ifFalse: [ commandsClasses ])
		collect: [ :pragma | pragma methodClass soleInstance ]
]

{ #category : #'*NewTools-Core' }
CmCommand class >> instantiateCommands: commandClasses for: aTool [

	^ commandClasses
		collect: [ :commandClass | commandClass forSpecContext: aTool ]
		thenSelect: [ :debugCommand | debugCommand decoratedCommand appliesTo: aTool ]
]

{ #category : #'*NewTools-Core' }
CmCommand class >> instantiateCommandsFromPragma: pragmaName for: aTool [

	^ self 
		instantiateCommands: (self commandClassesFromPragma: pragmaName) 
		for: aTool
]
