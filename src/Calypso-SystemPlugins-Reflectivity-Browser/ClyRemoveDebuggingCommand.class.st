"
I am a base class for commands which remove particular kind of metalink from given method or source node.
My subclasses should implement following class side methods: 

- metalinkManagerClass
It is a class which manages target type of metalinks. For example Breakpoint or ExecutionCounter.

- contextMenuOrder
It should return order in context menu
"
Class {
	#name : #ClyRemoveDebuggingCommand,
	#superclass : #ClyDebuggingCommand,
	#category : #'Calypso-SystemPlugins-Reflectivity-Browser-Commands'
}

{ #category : #testing }
ClyRemoveDebuggingCommand class >> isAbstract [
	^self = ClyRemoveDebuggingCommand 
]

{ #category : #activation }
ClyRemoveDebuggingCommand class >> methodBrowserTableIconActivation [
	<classAnnotation>
	
	^ClyTableIconCommandActivation priority: 900 for: ClyMethod asCalypsoItemContext
]

{ #category : #execution }
ClyRemoveDebuggingCommand >> debuggingToolClass [
	self subclassResponsibility 
]

{ #category : #accessing }
ClyRemoveDebuggingCommand >> defaultMenuIconName [
	^#smallCancel
]

{ #category : #accessing }
ClyRemoveDebuggingCommand >> defaultMenuItemName [ 
	^'Remove ', self debuggingToolClass name asLowercase
]

{ #category : #execution }
ClyRemoveDebuggingCommand >> execute [
	
	sourceNode nodesWithLinks do: [:each | 
		self debuggingToolClass removeFrom: each]
]
