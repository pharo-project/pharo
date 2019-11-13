"
Description
--------------------

Demo for the labelled presenter
"
Class {
	#name : #SpDemoLabelledPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'email',
		'password'
	],
	#category : #'Spec2-Examples-Demo-Forms'
}

{ #category : #initialization }
SpDemoLabelledPresenter class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #email withConstraints: [ :constraints | constraints height: self labelledPresenterHeight ];
		add: #password withConstraints: [ :constraints | constraints height: self labelledPresenterHeight ];
		yourself
]

{ #category : #accessing }
SpDemoLabelledPresenter >> email [
	^ email
]

{ #category : #initialization }
SpDemoLabelledPresenter >> initializePresenters [
	| passwordInput |
	passwordInput := self newTextInput
		bePassword;
		yourself.
		
	email := self instantiate: (SpLabelledPresenter label: 'Email' input: self newTextInput description: 'Email you will use to send activations mails to your users.').
	password := self instantiate: (SpLabelledPresenter label: 'Password' input: passwordInput description: 'Password of the email account used.').
	
	self focusOrder add: email; add: password
]

{ #category : #accessing }
SpDemoLabelledPresenter >> password [
	^ password
]
