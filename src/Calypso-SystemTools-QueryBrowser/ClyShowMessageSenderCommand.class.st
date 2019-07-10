"
I am a command to show all message senders.

To show senders I ask the browser to spawn new query browser.

Internal Representation and Key Implementation Points.

    Instance Variables
	browser:		<ClyBrowser>
"
Class {
	#name : #ClyShowMessageSenderCommand,
	#superclass : #SycMessageCommand,
	#instVars : [
		'browser'
	],
	#category : #'Calypso-SystemTools-QueryBrowser-Commands-Queries'
}

{ #category : #'instance creation' }
ClyShowMessageSenderCommand class >> forMethods: methods by: aBrowser [ 
	^(self forMethods: methods) 
		browser: aBrowser
]

{ #category : #activation }
ClyShowMessageSenderCommand class >> methodBrowserToolbarActivation [
	<classAnnotation>
	
	^ClyToolbarActivation byItemOf: ClyButtonToolbarGroup for: ClyMethod asCalypsoItemContext
]

{ #category : #activation }
ClyShowMessageSenderCommand class >> methodMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byItemOf: ClyQueryMenuGroup for: ClyMethod asCalypsoItemContext 
]

{ #category : #activation }
ClyShowMessageSenderCommand class >> methodShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: $n meta for: ClyMethod asCalypsoItemContext
]

{ #category : #activation }
ClyShowMessageSenderCommand class >> sourceCodeMenuActivation [
	<classAnnotation>
	
	^SycSourceCodeMenuActivation byItemOf: ClyQueryMenuGroup for: ClySourceCodeContext
]

{ #category : #activation }
ClyShowMessageSenderCommand class >> sourceCodeShortcutActivation [
	"it is not supported yet. 
	One reason is that common text morph shorcuts are in priority.
	Another reason is that more clever logic needs to be implemented in ClySourceCodeContext to retrivev selected message from text selection"
	
	"<commandActivator>
	
	^ClyShortcutCommandActivator by: $n meta for: ClySourceCodeContext"
]

{ #category : #accessing }
ClyShowMessageSenderCommand >> browser [
	^ browser
]

{ #category : #accessing }
ClyShowMessageSenderCommand >> browser: anObject [
	browser := anObject
]

{ #category : #accessing }
ClyShowMessageSenderCommand >> defaultMenuItemName [
	^'Senders'
]

{ #category : #execution }
ClyShowMessageSenderCommand >> execute [
	
	| selectors |
	selectors := messages collect: [ :each | each selector].
	
	browser spawnQueryBrowserOn: (ClyMessageSendersQuery ofAny: selectors)
]

{ #category : #execution }
ClyShowMessageSenderCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	browser := aToolContext browser
]
