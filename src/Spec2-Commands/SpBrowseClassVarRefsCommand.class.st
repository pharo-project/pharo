"
My context is a class, I browse the class var refs of that class.
The system navigation is in charge of executing, and will ask the user to select a variable.
"
Class {
	#name : #SpBrowseClassVarRefsCommand,
	#superclass : #SpSystemNavigationCommand,
	#category : #'Spec2-Commands-Navigation'
}

{ #category : #default }
SpBrowseClassVarRefsCommand class >> defaultDescription [
	^ 'Browse the references to class variables'
]

{ #category : #default }
SpBrowseClassVarRefsCommand class >> shortName [

	^ 'class variables reference'
]

{ #category : #executing }
SpBrowseClassVarRefsCommand >> execute [	
	
	self systemNavigation browseClassVarRefs: self target
]
