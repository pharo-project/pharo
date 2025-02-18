"
I am a command to show all variable references.

To show references I ask the browser to spawn new query browser.

Internal Representation and Key Implementation Points.

    Instance Variables
	browser:		<ClyBrowser>
"
Class {
	#name : 'ClyShowVariableRefCommand',
	#superclass : 'SycVariableCommand',
	#instVars : [
		'browser'
	],
	#category : 'Calypso-SystemTools-QueryBrowser-Commands-Queries',
	#package : 'Calypso-SystemTools-QueryBrowser',
	#tag : 'Commands-Queries'
}

{ #category : 'activation' }
ClyShowVariableRefCommand class >> fullBrowserMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byItemOf: ClyQueryMenuGroup order: 2000 for: ClyFullBrowserVariableContext
]

{ #category : 'activation' }
ClyShowVariableRefCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation by: $n meta for: ClyFullBrowserVariableContext
]

{ #category : 'activation' }
ClyShowVariableRefCommand class >> sourceCodeMenuActivation [
	<classAnnotation>

	^SycSourceCodeMenuActivation byItemOf: ClyQueryMenuGroup for: ClySourceCodeContext
]

{ #category : 'accessing' }
ClyShowVariableRefCommand >> defaultMenuItemName [
	^'References'
]

{ #category : 'accessing' }
ClyShowVariableRefCommand >> description [

	^ 'Show all variable references.'
]

{ #category : 'execution' }
ClyShowVariableRefCommand >> execute [

	browser spawnQueryBrowserOn: (ClyVariableReferencesQuery ofAny: variables)
]

{ #category : 'execution' }
ClyShowVariableRefCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.

	browser := aToolContext browser
]
