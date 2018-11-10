"
I implement the basic behavior of all the dialogs 
to ask / add / edit credentials.

There are subclasses to each of the actions and the types of keys.
"
Class {
	#name : #IceTipAbstractAskCredentialsModel,
	#superclass : #ComposablePresenter,
	#instVars : [
		'storeCheckbox',
		'credentialStore',
		'accepted',
		'remoteHostname'
	],
	#category : #'Iceberg-TipUI-Credentials'
}

{ #category : #actions }
IceTipAbstractAskCredentialsModel >> cancelAction [
	^ self subclassResponsibility 
]

{ #category : #actions }
IceTipAbstractAskCredentialsModel >> closeAction [
	accepted ifFalse: [ self cancelAction ]
]

{ #category : #accessing }
IceTipAbstractAskCredentialsModel >> credentialStore: aStore [ 
	credentialStore := aStore
]

{ #category : #accessing }
IceTipAbstractAskCredentialsModel >> credentials [
	^ self subclassResponsibility 
]

{ #category : #initialization }
IceTipAbstractAskCredentialsModel >> initialize [
	super initialize.
	accepted := false
]

{ #category : #initialization }
IceTipAbstractAskCredentialsModel >> initializePresenter [

	super initializePresenter.
	self putFocusOrder
]

{ #category : #initialization }
IceTipAbstractAskCredentialsModel >> initializeWidgets [
	
	storeCheckbox := self newCheckBox
		label: 'Do you want this credentials to be stored for future use?';
		help: 'Please note that storing your credentials is handy but unsafe';
		state: true.
	
	self whenWindowChanged: [ self updateOkButton ].

]

{ #category : #testing }
IceTipAbstractAskCredentialsModel >> isOkEnabled [
	
	self subclassResponsibility
]

{ #category : #actions }
IceTipAbstractAskCredentialsModel >> okAction [
	^ self subclassResponsibility 
]

{ #category : #'API - opening' }
IceTipAbstractAskCredentialsModel >> open [
	self
		openNonModal
		modalRelativeTo: World.
	
	^ self credentials
]

{ #category : #'API - opening' }
IceTipAbstractAskCredentialsModel >> openNonModal [
	^ self
		openDialogWithSpec
		okAction: [self okAction];
		cancelAction: [self cancelAction];
		whenClosedDo: [self closeAction];	
		centered
		
]

{ #category : #initialization }
IceTipAbstractAskCredentialsModel >> putFocusOrder [
	^ self subclassResponsibility 
]

{ #category : #accessing }
IceTipAbstractAskCredentialsModel >> remoteHostname: anObject [

	remoteHostname := anObject

]

{ #category : #accessing }
IceTipAbstractAskCredentialsModel >> storeCheckbox [
	^ storeCheckbox 
]

{ #category : #accessing }
IceTipAbstractAskCredentialsModel >> storeCredentials [
	^ storeCheckbox state 
]

{ #category : #updating }
IceTipAbstractAskCredentialsModel >> updateOkButton [
	
	self window ifNil: [ ^ self ].
	self window toolbar ifNil: [ ^ self ].
	
	self window toolbar okButton enabled: (self isOkEnabled)
]
