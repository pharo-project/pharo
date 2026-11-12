"
Show all message senders.

To show senders I ask the browser to spawn new query browser.

Internal Representation and Key Implementation Points.

    Instance Variables
	browser:		<ClyBrowserMorph>
"
Class {
	#name : 'ClyShowMessageSenderCommand',
	#superclass : 'SycMessageCommand',
	#instVars : [
		'browser'
	],
	#category : 'Calypso-SystemTools-QueryBrowser-Commands-Queries',
	#package : 'Calypso-SystemTools-QueryBrowser',
	#tag : 'Commands-Queries'
}

{ #category : 'instance creation' }
ClyShowMessageSenderCommand class >> forMethods: methods by: aBrowser [
	^(self forMethods: methods)
		browser: aBrowser
]

{ #category : 'activation' }
ClyShowMessageSenderCommand class >> sourceCodeShortcutActivation [
	"it is not supported yet.
	One reason is that common text morph shorcuts are in priority.
	Another reason is that more clever logic needs to be implemented in ClySourceCodeContext to retrivev selected message from text selection"

	"<commandActivator>

	^ClyShortcutCommandActivator by: $n meta for: ClySourceCodeContext"
]

{ #category : 'accessing' }
ClyShowMessageSenderCommand >> browser [
	^ browser
]

{ #category : 'accessing' }
ClyShowMessageSenderCommand >> browser: anObject [
	browser := anObject
]

{ #category : 'execution' }
ClyShowMessageSenderCommand >> executeRefactoring [

	(Smalltalk tools toolNamed: #messageList) browseSendersOfAll: (messages collect: [ :message | message selector ])
]

{ #category : 'accessing' }
ClyShowMessageSenderCommand >> iconName [
	^ #smallFind
]

{ #category : 'accessing' }
ClyShowMessageSenderCommand >> name [
	^'Senders'
]

{ #category : 'accessing' }
ClyShowMessageSenderCommand >> order [

	^ 1000
]

{ #category : 'execution' }
ClyShowMessageSenderCommand >> prepareFullExecution [

	super prepareFullExecution.
	browser := self context browser
]
