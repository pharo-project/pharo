"
I am a command to show all message implementors.

To show implementors I ask the browser to spawn new query browser.

Internal Representation and Key Implementation Points.

    Instance Variables
	browser:		<ClyBrowser>
"
Class {
	#name : #ClyShowMessageImplementorCommand,
	#superclass : #SycMessageCommand,
	#instVars : [
		'browser'
	],
	#category : #'Calypso-SystemTools-QueryBrowser-Commands-Queries'
}

{ #category : #activation }
ClyShowMessageImplementorCommand class >> methodBrowserToolbarActivation [
	<classAnnotation>
	
	^ClyToolbarActivation byItemOf: ClyButtonToolbarGroup for: ClyMethod asCalypsoItemContext
]

{ #category : #activation }
ClyShowMessageImplementorCommand class >> methodMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byItemOf: ClyQueryMenuGroup for: ClyMethod asCalypsoItemContext
]

{ #category : #activation }
ClyShowMessageImplementorCommand class >> methodShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: $m meta for: ClyMethod asCalypsoItemContext
]

{ #category : #activation }
ClyShowMessageImplementorCommand class >> sourceCodeMenuActivation [
	<classAnnotation>
	
	^SycSourceCodeMenuActivation byItemOf: ClyQueryMenuGroup for: ClySourceCodeContext
]

{ #category : #activation }
ClyShowMessageImplementorCommand class >> sourceCodeShortcutActivation [
	"it is not supported yet. Look senders command"

	"<commandActivator>
	
	^ClyShortcutCommandActivator by: $m meta for: ClySourceCodeContext"
]

{ #category : #accessing }
ClyShowMessageImplementorCommand >> defaultMenuItemName [
	^'Implementors'
]

{ #category : #execution }
ClyShowMessageImplementorCommand >> execute [

	| selectors |
	selectors := messages collect: [ :each | each selector].
	
	browser spawnQueryBrowserOn: (ClyMessageImplementorsQuery ofAny: selectors)
]

{ #category : #execution }
ClyShowMessageImplementorCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	browser := aToolContext browser
]
