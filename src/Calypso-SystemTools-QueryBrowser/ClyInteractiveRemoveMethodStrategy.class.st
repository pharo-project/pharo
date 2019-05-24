"
I am a base class for remove method strategies which request some additional information from the user.

Strategies should be created with browser which is used by subclasses for user requests: 

	ClyInteractiveRemoveMethodStrategy newForBrowser: aBrowser
 
Internal Representation and Key Implementation Points.

    Instance Variables
	browser:		<Object>
"
Class {
	#name : #ClyInteractiveRemoveMethodStrategy,
	#superclass : #SycRemoveMethodStrategy,
	#instVars : [
		'browser'
	],
	#category : #'Calypso-SystemTools-QueryBrowser-Commands-Methods'
}

{ #category : #'instance creation' }
ClyInteractiveRemoveMethodStrategy class >> newForBrowser: aBrowser [

	^self new 
		browser: aBrowser
]

{ #category : #accessing }
ClyInteractiveRemoveMethodStrategy >> browser [
	^ browser
]

{ #category : #accessing }
ClyInteractiveRemoveMethodStrategy >> browser: anObject [
	browser := anObject
]
