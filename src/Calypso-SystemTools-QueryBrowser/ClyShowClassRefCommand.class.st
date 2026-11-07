"
I am a command to show all class references

To show references I ask the browser to spawn new query browser.

Internal Representation and Key Implementation Points.

    Instance Variables
	browser:		<ClyBrowser>
"
Class {
	#name : 'ClyShowClassRefCommand',
	#superclass : 'SycClassCommand',
	#instVars : [
		'browser'
	],
	#category : 'Calypso-SystemTools-QueryBrowser-Commands-Queries',
	#package : 'Calypso-SystemTools-QueryBrowser',
	#tag : 'Commands-Queries'
}

{ #category : 'instance creation' }
ClyShowClassRefCommand class >> forClasses: classes by: aBrowser [
	^(self for: classes)
		browser: aBrowser
]

{ #category : 'activation' }
ClyShowClassRefCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^ClyShortcutActivation by: $n meta for: ClyFullBrowserClassContext
]

{ #category : 'activation' }
ClyShowClassRefCommand class >> fullBrowserToolbarActivation [
	<classAnnotation>

	^ClyToolbarActivation byItemOf: ClyButtonToolbarGroup for: ClyFullBrowserClassContext
]

{ #category : 'activation' }
ClyShowClassRefCommand class >> queryBrowserShortcutActivation [
	<classAnnotation>

	^ClyShortcutActivation by: $c meta, $n meta for: ClyQueryBrowserContext
]

{ #category : 'activation' }
ClyShowClassRefCommand class >> queryBrowserToolbarActivation [
	<classAnnotation>

	^ClyToolbarActivation byItemOf: ClyButtonToolbarGroup for: ClyQueryBrowserContext
]

{ #category : 'accessing' }
ClyShowClassRefCommand >> browser [

	^ browser
]

{ #category : 'accessing' }
ClyShowClassRefCommand >> browser: anObject [

	browser := anObject
]

{ #category : 'execution' }
ClyShowClassRefCommand >> executeRefactoring [

	(Smalltalk tools toolNamed: #messageList) browseReferencesToClass: classes first
]

{ #category : 'testing' }
ClyShowClassRefCommand >> isAvailableInMenuOf: aToolContext [

	^ aToolContext isFullBrowserClassContext or: [ aToolContext isQueryBrowserContext or: [ aToolContext isSourceCodeContext ] ]
]

{ #category : 'accessing' }
ClyShowClassRefCommand >> menuGroup [

	^ ClyQueryMenuGroup
]

{ #category : 'accessing' }
ClyShowClassRefCommand >> name [
	^'Class refs.'
]

{ #category : 'accessing' }
ClyShowClassRefCommand >> order [

	^ 1000
]

{ #category : 'execution' }
ClyShowClassRefCommand >> prepareFullExecution [

	super prepareFullExecution.
	browser := self context browser
]
