"
Description
--------------------

I am a class only here to help to migrate from Spec-Pharo70 to Spec-Pharo80.

I will catch methods from the old dialog toolbar implementation and use the new API.
"
Class {
	#name : #DeprecatedSpecToolbarHelper,
	#superclass : #Object,
	#instVars : [
		'windowPresenter'
	],
	#category : #'Spec-Deprecated80-Obsolete'
}

{ #category : #'instance creation' }
DeprecatedSpecToolbarHelper class >> for: aWindowPresenter [
	^ self new
		windowPresenter: aWindowPresenter;
		yourself
]

{ #category : #deprecation }
DeprecatedSpecToolbarHelper class >> isDeprecated [
	^ true
]

{ #category : #deprecated }
DeprecatedSpecToolbarHelper >> cancelButton [
	^ windowPresenter
		addButton: 'Cancel'
		do: [ :presenter | 
			presenter triggerCancelAction.
			presenter close ]
]

{ #category : #deprecated }
DeprecatedSpecToolbarHelper >> okButton [
	^ windowPresenter
		addButton: 'Ok'
		do: [ :presenter | 
			presenter triggerOkAction.
			presenter close ]
]

{ #category : #deprecated }
DeprecatedSpecToolbarHelper >> windowPresenter: aDialogWindowPresenter [ 
	windowPresenter := aDialogWindowPresenter
]
