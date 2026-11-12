"
Show all message implementors.

To show implementors I ask the browser to spawn new query browser.

Internal Representation and Key Implementation Points.

    Instance Variables
	browser:		<ClyBrowserMorph>
"
Class {
	#name : 'ClyShowMessageImplementorCommand',
	#superclass : 'SycMessageCommand',
	#instVars : [
		'browser'
	],
	#category : 'Calypso-SystemTools-QueryBrowser-Commands-Queries',
	#package : 'Calypso-SystemTools-QueryBrowser',
	#tag : 'Commands-Queries'
}

{ #category : 'activation' }
ClyShowMessageImplementorCommand class >> sourceCodeShortcutActivation [
	"it is not supported yet. Look senders command"

	"<commandActivator>

	^ClyShortcutCommandActivator by: $m meta for: ClySourceCodeContext"
]

{ #category : 'execution' }
ClyShowMessageImplementorCommand >> executeRefactoring [

	(Smalltalk tools toolNamed: #messageList) browseImplementorsOfAll: (messages collect: [ :message | message selector ])
]

{ #category : 'accessing' }
ClyShowMessageImplementorCommand >> iconName [
	^ #smallFind
]

{ #category : 'accessing' }
ClyShowMessageImplementorCommand >> name [
	^'Implementors'
]

{ #category : 'accessing' }
ClyShowMessageImplementorCommand >> order [

	^ 1000
]

{ #category : 'execution' }
ClyShowMessageImplementorCommand >> prepareFullExecution [

	super prepareFullExecution.
	browser := self context browser
]
