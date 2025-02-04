"
I am a command to find class and show it in the browser.

By default I am executed by cmd+f shortcut
"
Class {
	#name : 'ClyFindClassCommand',
	#superclass : 'ClyBrowserCommand',
	#instVars : [
		'class'
	],
	#category : 'Calypso-SystemTools-Core-Commands-Classes',
	#package : 'Calypso-SystemTools-Core',
	#tag : 'Commands-Classes'
}

{ #category : 'accessing' }
ClyFindClassCommand >> defaultMenuIconName [
	^#smallFind
]

{ #category : 'accessing' }
ClyFindClassCommand >> defaultMenuItemName [
	^'Find class'
]

{ #category : 'execution' }
ClyFindClassCommand >> execute [

	browser selectClass: class
]

{ #category : 'execution' }
ClyFindClassCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.

	class := aToolContext requestSingleClass: 'Choose class'
]
