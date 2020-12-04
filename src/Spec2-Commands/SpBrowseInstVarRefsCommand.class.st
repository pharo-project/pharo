"
My context is a class, I browse the inst var refs of that class.
The system navigation is in charge of executing, and will ask the user to select a variable.
"
Class {
	#name : #SpBrowseInstVarRefsCommand,
	#superclass : #SpSystemNavigationCommand,
	#category : #'Spec2-Commands-Navigation'
}

{ #category : #default }
SpBrowseInstVarRefsCommand class >> defaultDescription [
	^ 'Browse the references to instance variables'
]

{ #category : #default }
SpBrowseInstVarRefsCommand class >> shortName [

	^ 'instance variables reference'
]

{ #category : #executing }
SpBrowseInstVarRefsCommand >> execute [	
	self systemNavigation browseInstVarRefs: self target
]
