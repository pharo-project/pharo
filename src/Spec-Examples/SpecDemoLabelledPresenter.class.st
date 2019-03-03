"
Description
--------------------

Demo for the labelled presenter
"
Class {
	#name : #SpecDemoLabelledPresenter,
	#superclass : #ComposablePresenter,
	#instVars : [
		'email',
		'password'
	],
	#category : #'Spec-Examples-Demo-Forms'
}

{ #category : #initialization }
SpecDemoLabelledPresenter class >> defaultSpec [
	^ SpecLayout composed
		newColumn: [ :col | 
			col
				add: #email height: self labelledPresenterHeight;
				add: #password height: self labelledPresenterHeight;
				addSpacer ];
		yourself
]

{ #category : #accessing }
SpecDemoLabelledPresenter >> email [
	^ email
]

{ #category : #initialization }
SpecDemoLabelledPresenter >> initializeWidgets [
	| passwordInput |
	passwordInput := self newTextInput
		encrypted: true;
		yourself.
		
	email := self instantiate: (LabelledPresenter label: 'Email' input: self newTextInput description: 'Email you will use to send activations mails to your users.').
	password := self instantiate: (LabelledPresenter label: 'Password' input: passwordInput description: 'Password of the email account used.').
	
	self focusOrder add: email; add: password
]

{ #category : #accessing }
SpecDemoLabelledPresenter >> password [
	^ password
]
