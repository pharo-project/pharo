"
My context is a class, I browse the class variables of that class.
"
Class {
	#name : #SpBrowseClassVariablesCommand,
	#superclass : #SpSystemNavigationCommand,
	#category : #'Spec2-Commands-Navigation'
}

{ #category : #default }
SpBrowseClassVariablesCommand class >> defaultDescription [
	^ 'Browse the class variables'
]

{ #category : #default }
SpBrowseClassVariablesCommand class >> shortName [

	^ 'class variables'
]

{ #category : #executing }
SpBrowseClassVariablesCommand >> execute [	
	self systemNavigation browseClassVariables: self context
]
