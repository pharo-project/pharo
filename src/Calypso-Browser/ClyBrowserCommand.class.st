"
I am the root of the command hierarchy which affects state of the browser.

Instance Variables
	browser:		<ClyBrowser>
"
Class {
	#name : 'ClyBrowserCommand',
	#superclass : 'ClyCommand',
	#instVars : [
		'browser'
	],
	#category : 'Calypso-Browser-Commands',
	#package : 'Calypso-Browser',
	#tag : 'Commands'
}

{ #category : 'accessing' }
ClyBrowserCommand >> browser [
	^ browser ifNil: [ self context ifNotNil: [ :ctx | ctx browser ] ]
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
