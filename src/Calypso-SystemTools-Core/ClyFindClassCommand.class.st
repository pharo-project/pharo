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

{ #category : 'execution' }
ClyFindClassCommand >> execute [

	browser selectClass: class
]

{ #category : 'accessing' }
ClyFindClassCommand >> iconName [
	^#smallFind
]

{ #category : 'accessing' }
ClyFindClassCommand >> name [
	^'Find class'
]

{ #category : 'execution' }
ClyFindClassCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.

	class := aToolContext requestSingleClass: 'Choose class'
]
