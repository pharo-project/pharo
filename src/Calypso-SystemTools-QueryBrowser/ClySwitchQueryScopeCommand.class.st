"
I am a command to switch the current scope of the query in the query browser.

Currently I support only context menu and toolbar activation where I retrieve available navigation scopes from the browser to allow user select one of them.

Internal Representation and Key Implementation Points.

    Instance Variables
	queryScope:		<ClyScope>
"
Class {
	#name : #ClySwitchQueryScopeCommand,
	#superclass : #ClySwitchBrowserModeCommand,
	#instVars : [
		'queryScope'
	],
	#category : #'Calypso-SystemTools-QueryBrowser-Commands-Controlling'
}

{ #category : #activation }
ClySwitchQueryScopeCommand class >> queryBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byItemOf: ClySwitchScopeMenuGroup for: ClyQueryBrowserContext 
]

{ #category : #activation }
ClySwitchQueryScopeCommand class >> queryBrowserToolbarActivation [
	<classAnnotation>
	
	^ClyToolbarActivation byItemOf: ClyQueryResultToolbarGroup order: -5 for: ClyQueryBrowserContext 
]

{ #category : #'instance creation' }
ClySwitchQueryScopeCommand class >> toScope: anEnvironmentScope in: aBrowser [
	^self new 
		browser: aBrowser;
		queryScope: anEnvironmentScope 
]

{ #category : #'activation-toolbar support' }
ClySwitchQueryScopeCommand >> addToolbarItemInto: toolbarMorph using: aCommandActivator [
	
	| compoBox |
	compoBox := ClyScopeCompoBox for: browser.
	
	toolbarMorph addNewItem: compoBox
]

{ #category : #accessing }
ClySwitchQueryScopeCommand >> defaultMenuItemName [
	queryScope ifNil: [ ^'Switch scope' ].
	
	^'Switch scope to ', queryScope description
]

{ #category : #accessing }
ClySwitchQueryScopeCommand >> defaultToolbarItemName [
	
	^queryScope description
]

{ #category : #execution }
ClySwitchQueryScopeCommand >> execute [
	browser switchScopeTo: queryScope
]

{ #category : #'activation-context menu' }
ClySwitchQueryScopeCommand >> fillContextMenu: aMenu using: aCommandActivator [
	| concreteScopeCommand switchActivator |
	
	browser allNavigationScopes do: [ :newScope |
		concreteScopeCommand := self class toScope: newScope in: browser.
		switchActivator := aCommandActivator newForCommand: concreteScopeCommand.
		aMenu 	add: switchActivator menuItemName target: switchActivator selector: #executeCommand]
]

{ #category : #testing }
ClySwitchQueryScopeCommand >> isAppliedToBrowser [
	^browser isQueryScopeActive: queryScope
]

{ #category : #accessing }
ClySwitchQueryScopeCommand >> queryScope [
	^ queryScope
]

{ #category : #accessing }
ClySwitchQueryScopeCommand >> queryScope: anObject [
	queryScope := anObject
]
