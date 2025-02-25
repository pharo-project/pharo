"
I am a command to show all variable writers (the methods which assigns new value to the variables).

To show writers I ask the browser to spawn new query browser.

Internal Representation and Key Implementation Points.

    Instance Variables
	browser:		<ClyBrowser>
"
Class {
	#name : 'ClyShowVariableWriterCommand',
	#superclass : 'SycVariableCommand',
	#instVars : [
		'browser'
	],
	#category : 'Calypso-SystemTools-QueryBrowser-Commands-Queries',
	#package : 'Calypso-SystemTools-QueryBrowser',
	#tag : 'Commands-Queries'
}

{ #category : 'activation' }
ClyShowVariableWriterCommand class >> fullBrowserMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byItemOf: ClyQueryMenuGroup order: 2001 for: ClyFullBrowserVariableContext
]

{ #category : 'activation' }
ClyShowVariableWriterCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation by: $m meta for: ClyFullBrowserVariableContext
]

{ #category : 'activation' }
ClyShowVariableWriterCommand class >> sourceCodeMenuActivation [
	<classAnnotation>

	^SycSourceCodeMenuActivation byItemOf: ClyQueryMenuGroup for: ClySourceCodeContext
]

{ #category : 'accessing' }
ClyShowVariableWriterCommand >> defaultMenuItemName [
	^'Writers'
]

{ #category : 'accessing' }
ClyShowVariableWriterCommand >> description [

	^ 'Show all variable writers.'
]

{ #category : 'execution' }
ClyShowVariableWriterCommand >> execute [

	browser spawnQueryBrowserOn: (ClyVariableWritersQuery ofAny: variables)
]

{ #category : 'execution' }
ClyShowVariableWriterCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.

	browser := aToolContext browser
]
