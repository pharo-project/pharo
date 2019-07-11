"
I am a pre debugger showing to the user textual informations. 

I'll typically display notifications.	
 
Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	textPresenter:		<aTextPresenter>	A presenter to display the notification.

"
Class {
	#name : #GTSpecPreDebugNotificationPresenter,
	#superclass : #GTSpecPreDebugPresenter,
	#instVars : [
		'textPresenter'
	],
	#category : #'GT-Debugger-UI'
}

{ #category : #specs }
GTSpecPreDebugNotificationPresenter class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #actions withConstraints: [ :constraints | constraints height: self toolbarHeight ];
		add: #textPresenter;
		yourself
]

{ #category : #initialization }
GTSpecPreDebugNotificationPresenter >> initializeWidgets [
	super initializeWidgets.
	textPresenter := self newText.
	
	textPresenter disable.

	self focusOrder add: textPresenter
]

{ #category : #accessing }
GTSpecPreDebugNotificationPresenter >> message: aString [
	textPresenter text: aString
]
