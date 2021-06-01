Class {
	#name : #ClyShowTraitUsersCommand,
	#superclass : #SycClassCommand,
	#instVars : [
		'browser'
	],
	#category : #'Calypso-SystemTools-QueryBrowser-Commands-Queries'
}

{ #category : #'instance creation' }
ClyShowTraitUsersCommand class >> forClasses: classes by: aBrowser [ 
	^(self for: classes) 
		browser: aBrowser
]

{ #category : #activation }
ClyShowTraitUsersCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byItemOf: ClyQueryMenuGroup for: ClyFullBrowserClassContext
]

{ #category : #activation }
ClyShowTraitUsersCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: $n meta for: ClyFullBrowserClassContext
]

{ #category : #activation }
ClyShowTraitUsersCommand class >> fullBrowserToolbarActivation [
	<classAnnotation>
	
	^ClyToolbarActivation byItemOf: ClyButtonToolbarGroup for: ClyFullBrowserClassContext 
]

{ #category : #activation }
ClyShowTraitUsersCommand class >> queryBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byItemOf: ClyQueryMenuGroup for: ClyQueryBrowserContext 
]

{ #category : #activation }
ClyShowTraitUsersCommand class >> queryBrowserShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: $c meta, $n meta for: ClyQueryBrowserContext
]

{ #category : #activation }
ClyShowTraitUsersCommand class >> queryBrowserToolbarActivation [
	<classAnnotation>
	
	^ClyToolbarActivation byItemOf: ClyButtonToolbarGroup for: ClyQueryBrowserContext 
]

{ #category : #activation }
ClyShowTraitUsersCommand class >> sourceCodeMenuActivation [
	<classAnnotation>
	
	^SycSourceCodeMenuActivation byItemOf: ClyQueryMenuGroup for: ClySourceCodeContext
]

{ #category : #accessing }
ClyShowTraitUsersCommand >> browser [

	^ browser
]

{ #category : #accessing }
ClyShowTraitUsersCommand >> browser: anObject [

	browser := anObject
]

{ #category : #accessing }
ClyShowTraitUsersCommand >> defaultMenuItemName [
	^'Show trait users'
]

{ #category : #execution }
ClyShowTraitUsersCommand >> execute [

	browser spawnQueryBrowserOn:
		 (ClyClassReferencesQuery ofAny: classes result: classes first traitUsers)
]

{ #category : #execution }
ClyShowTraitUsersCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	browser := aToolContext browser
]
