"
I am a command to switch the current scope of the query in the query browser.

Currently I support only context menu and toolbar activation where I retrieve available navigation scopes from the browser to allow user select one of them.

Internal Representation and Key Implementation Points.

    Instance Variables
	queryScope:		<ClyScope>
"
Class {
	#name : 'ClySwitchQueryScopeCommand',
	#superclass : 'ClySwitchBrowserModeCommand',
	#instVars : [
		'queryScope'
	],
	#category : 'Calypso-SystemTools-QueryBrowser-Commands-Controlling',
	#package : 'Calypso-SystemTools-QueryBrowser',
	#tag : 'Commands-Controlling'
}

{ #category : 'activation' }
ClySwitchQueryScopeCommand class >> queryBrowserMenuActivation [
	<classAnnotation>

	^ClyContextMenuActivation byItemOf: ClySwitchScopeMenuGroup for: ClyQueryBrowserContext
]

{ #category : 'activation' }
ClySwitchQueryScopeCommand class >> queryBrowserToolbarActivation [
	<classAnnotation>

	^ClyToolbarActivation byItemOf: ClyQueryResultToolbarGroup order: -5 for: ClyQueryBrowserContext
]

{ #category : 'instance creation' }
ClySwitchQueryScopeCommand class >> toScope: anEnvironmentScope in: aBrowser [
	^self new
		browser: aBrowser;
		queryScope: anEnvironmentScope
]

{ #category : 'toolbar' }
ClySwitchQueryScopeCommand >> decoratesToolbarItem [
	"The scope combo box is not decorated."
	^ false
]

{ #category : 'accessing' }
ClySwitchQueryScopeCommand >> defaultToolbarItemName [

	^queryScope description
]

{ #category : 'execution' }
ClySwitchQueryScopeCommand >> executeRefactoring [
	self browser switchScopeTo: queryScope
]

{ #category : 'testing' }
ClySwitchQueryScopeCommand >> isAppliedToBrowser [
	^self browser isQueryScopeActive: queryScope
]

{ #category : 'menu' }
ClySwitchQueryScopeCommand >> menuAlternatives [
	"One switch command per navigation scope of the browser."
	^ self browser allNavigationScopes collect: [ :newScope |
		self class toScope: newScope in: self browser ]
]

{ #category : 'accessing' }
ClySwitchQueryScopeCommand >> name [
	queryScope ifNil: [ ^'Switch scope' ].

	^'Switch scope to ', queryScope description
]

{ #category : 'accessing' }
ClySwitchQueryScopeCommand >> queryScope [
	^ queryScope
]

{ #category : 'accessing' }
ClySwitchQueryScopeCommand >> queryScope: anObject [
	queryScope := anObject
]

{ #category : 'toolbar' }
ClySwitchQueryScopeCommand >> toolbarItemFor: aCommandActivator [
	^ ClyScopeCompoBoxMorph for: self browser
]
