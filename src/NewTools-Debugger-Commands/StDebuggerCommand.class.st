"
I command actions that can be requested from a debugger
"
Class {
	#name : #StDebuggerCommand,
	#superclass : #CmCommand,
	#category : #'NewTools-Debugger-Commands'
}

{ #category : #'instance creation' }
StDebuggerCommand class >> codeContextMenuCommandsForDebugger: debugger [

	^ self
		instantiateCommandsFromPragma: #CodeContextMenuDebugCommand
		for: debugger
]

{ #category : #initialization }
StDebuggerCommand class >> defaultIconName [
	^ self subclassResponsibility
]

{ #category : #initialization }
StDebuggerCommand class >> defaultShortcut [
	^ self subclassResponsibility
]

{ #category : #'instance creation' }
StDebuggerCommand class >> forSpecContext: anObject [
	^ (self
		forSpecWithIconNamed: self defaultIconName
		shortcutKey: self defaultShortcut)
		context: anObject;
		yourself
]

{ #category : #default }
StDebuggerCommand class >> shortName [
	^self defaultName
]

{ #category : #'instance creation' }
StDebuggerCommand class >> toolbarCommandsForDebugger: debugger [

	^ self
		instantiateCommandsFromPragma: #toolbarDebugCommand:
		for: debugger
]

{ #category : #comparing }
StDebuggerCommand >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ context = anObject context
		and: [ basicName = anObject name ]
]

{ #category : #testing }
StDebuggerCommand >> appliesTo: aDebugger [
	^ aDebugger canExecuteDebugCommand
]

{ #category : #converting }
StDebuggerCommand >> asSpecCommand [
	
	^ super asSpecCommand
		iconProvider: StPharoApplication current iconProvider;
		yourself
]

{ #category : #accessing }
StDebuggerCommand >> canBeExecuted [
	^ self appliesTo: self debugger
]

{ #category : #accessing }
StDebuggerCommand >> debugger [
	^self context
]

{ #category : #comparing }
StDebuggerCommand >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ context hash bitXor: basicName hash
]

{ #category : #initialization }
StDebuggerCommand >> initialize [

	super initialize.
	self description: (String streamContents: [ :stream | 
		stream 
			<< self class defaultName
			<< ': '
			<< self class defaultDescription ])
]
