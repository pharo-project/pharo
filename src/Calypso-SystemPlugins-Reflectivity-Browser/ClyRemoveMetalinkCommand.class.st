"
I am a base class for commands which remove particular kind of metalink from given method or source node.
My subclasses should implement following class side methods: 

- metalinkManagerClass
It is a class which manages target type of metalinks. For example Breakpoint or ExecutionCounter.

- contextMenuOrder
It should return order in context menu
"
Class {
	#name : #ClyRemoveMetalinkCommand,
	#superclass : #ClyMetalinkCommand,
	#category : #'Calypso-SystemPlugins-Reflectivity-Browser-Commands'
}

{ #category : #testing }
ClyRemoveMetalinkCommand class >> isAbstract [
	^self = ClyRemoveMetalinkCommand 
]

{ #category : #activation }
ClyRemoveMetalinkCommand class >> methodBrowserTableIconActivation [
	<classAnnotation>
	
	^ClyTableIconCommandActivation priority: 900 for: ClyMethod asCalypsoItemContext
]

{ #category : #accessing }
ClyRemoveMetalinkCommand >> defaultMenuIconName [
	^#smallCancel
]

{ #category : #accessing }
ClyRemoveMetalinkCommand >> defaultMenuItemName [ 
	^'Remove ', self metalinkManagerClass name asLowercase
]

{ #category : #execution }
ClyRemoveMetalinkCommand >> execute [
	
	sourceNode nodesWithLinks do: [:each | 
		self metalinkManagerClass removeFrom: each]
]

{ #category : #execution }
ClyRemoveMetalinkCommand >> metalinkManagerClass [
	self subclassResponsibility 
]
