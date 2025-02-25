"
I am the root of the command hierarchy which affects state of the browser.

Instance Variables
	browser:		<ClyBrowser>
"
Class {
	#name : 'ClyBrowserCommand',
	#superclass : 'CmdCommand',
	#instVars : [
		'browser'
	],
	#category : 'Calypso-Browser-Commands',
	#package : 'Calypso-Browser',
	#tag : 'Commands'
}

{ #category : 'testing' }
ClyBrowserCommand class >> isAbstract [
	^self = ClyBrowserCommand
]

{ #category : 'accessing' }
ClyBrowserCommand >> browser [
	^ browser
]

{ #category : 'accessing' }
ClyBrowserCommand >> browser: anObject [
	browser := anObject
]

{ #category : 'accessing' }
ClyBrowserCommand >> description [
	"Answer a <String> describing the receiver, useful for instance in menu items pop-ups"

	^ self class comment.
]

{ #category : 'execution' }
ClyBrowserCommand >> readParametersFromContext: aToolContext [
	super readParametersFromContext: aToolContext.
	browser := aToolContext browser
]
