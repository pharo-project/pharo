"
I am the context in which the Calypso commands run. I hold the tool that owns the command and answer the questions that commands and their menus ask about the selection (classes, packages, methods, variables, source nodes). Concrete contexts (browser, text editor, critics) specialise me. Commands are declared on context classes, or on the types of the selected items, see #isDescribedByCommandAnchor: and the guide doc/Calypso/Commands.md of the Pharo repository.
"
Class {
	#name : 'ClyToolContext',
	#superclass : 'Object',
	#instVars : [
		'tool'
	],
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'commands' }
ClyToolContext class >> calypsoCommandsIn: aCommandMap [

	<calypsoCommands>
	(aCommandMap textLeftBarClick add: ClyOpenTextLeftBarMenuCommand) beYellowButton
]

{ #category : 'instance creation' }
ClyToolContext class >> for: aTool [
	^self new
		tool: aTool
]

{ #category : 'testing' }
ClyToolContext >> allowsExecutionOf: aCommandClass [

	^aCommandClass canBeExecutedInContext: self
]

{ #category : 'testing' }
ClyToolContext >> hasUnacceptedEdits [ 
	^ false 
]

{ #category : 'testing' }
ClyToolContext >> isClassDefinitionContext [

	^ false
]

{ #category : 'testing' }
ClyToolContext >> isDescribedByCommandAnchor: anAnchorClass [
	"The commands declared on an anchor apply to me when I am an instance of it or of its subclasses. See ClyCommandMap."

	^ self class includesBehavior: anAnchorClass
]

{ #category : 'testing' }
ClyToolContext >> isFullBrowserClassContext [

	^ false
]

{ #category : 'testing' }
ClyToolContext >> isFullBrowserClassGroupContext [

	^ false
]

{ #category : 'testing' }
ClyToolContext >> isFullBrowserMethodContext [

	^ false
]

{ #category : 'testing' }
ClyToolContext >> isFullBrowserMethodGroupContext [

	^ false
]

{ #category : 'testing' }
ClyToolContext >> isFullBrowserPackageContext [

	^ false
]

{ #category : 'testing' }
ClyToolContext >> isFullBrowserVariableContext [

	^ false
]

{ #category : 'testing' }
ClyToolContext >> isMethodSourceContext [

	^ false
]

{ #category : 'testing' }
ClyToolContext >> isQueryBrowserContext [

	^ false
]

{ #category : 'testing' }
ClyToolContext >> isSimilarTo: anotherToolContext [
	^self = anotherToolContext
]

{ #category : 'testing' }
ClyToolContext >> isSourceCodeContext [

	^ false
]

{ #category : 'ui requests' }
ClyToolContext >> openContextMenu: aMenu [

	tool openContextMenu: aMenu
]

{ #category : 'command execution' }
ClyToolContext >> processFailure: anException of: aCommand [
	anException notifyUserOfCommand: aCommand
]

{ #category : 'accessing' }
ClyToolContext >> tool [
	^ tool
]

{ #category : 'accessing' }
ClyToolContext >> tool: anObject [
	tool := anObject
]
