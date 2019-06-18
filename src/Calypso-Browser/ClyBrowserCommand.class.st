"
I am the root of the command hierarchy which affects state of the browser.

Instance Variables
	browser:		<ClyBrowser>
"
Class {
	#name : #ClyBrowserCommand,
	#superclass : #CmdCommand,
	#instVars : [
		'browser'
	],
	#category : #'Calypso-Browser-Commands'
}

{ #category : #testing }
ClyBrowserCommand class >> isAbstract [
	^self = ClyBrowserCommand 
]

{ #category : #accessing }
ClyBrowserCommand >> browser [
	^ browser
]

{ #category : #accessing }
ClyBrowserCommand >> browser: anObject [
	browser := anObject
]

{ #category : #execution }
ClyBrowserCommand >> readParametersFromContext: aToolContext [
	super readParametersFromContext: aToolContext.
	browser := aToolContext browser
]
