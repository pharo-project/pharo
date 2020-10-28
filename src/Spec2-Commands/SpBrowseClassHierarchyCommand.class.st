"
My context must evaluate to a Behavior. I then browse the hierarchy of that behavior.
"
Class {
	#name : #SpBrowseClassHierarchyCommand,
	#superclass : #SpSystemSelectionCommand,
	#category : #'Spec2-Commands-Navigation'
}

{ #category : #default }
SpBrowseClassHierarchyCommand class >> defaultDescription [
	^ 'Browse the class hierarchy of the selected class'
]

{ #category : #default }
SpBrowseClassHierarchyCommand class >> shortName [

	^ 'class hierarchy'
]

{ #category : #executing }
SpBrowseClassHierarchyCommand >> execute [

	self context doBrowseHierarchy
]
