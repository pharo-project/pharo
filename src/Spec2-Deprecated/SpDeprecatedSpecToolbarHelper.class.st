"
Description
--------------------

I am a class only here to help to migrate from Spec-Pharo70 to Spec-Pharo80.

I will catch methods from the old dialog toolbar implementation and use the new API.
"
Class {
	#name : #SpDeprecatedSpecToolbarHelper,
	#superclass : #Object,
	#instVars : [
		'windowPresenter'
	],
	#category : #'Spec2-Deprecated-Obsolete'
}

{ #category : #'instance creation' }
SpDeprecatedSpecToolbarHelper class >> for: aWindowPresenter [
	^ self new
		windowPresenter: aWindowPresenter;
		yourself
]

{ #category : #deprecation }
SpDeprecatedSpecToolbarHelper class >> isDeprecated [
	^ true
]

{ #category : #deprecated }
SpDeprecatedSpecToolbarHelper >> cancelButton [
	^ windowPresenter
		addButton: 'Cancel'
		do: [ :presenter | 
			presenter triggerCancelAction.
			presenter close ]
]

{ #category : #deprecated }
SpDeprecatedSpecToolbarHelper >> okButton [
	^ windowPresenter
		addButton: 'Ok'
		do: [ :presenter | 
			presenter triggerOkAction.
			presenter close ]
]

{ #category : #deprecated }
SpDeprecatedSpecToolbarHelper >> windowPresenter: aDialogWindowPresenter [ 
	windowPresenter := aDialogWindowPresenter
]
