"
Browse all references to the class returned by the evaluation of the context.
"
Class {
	#name : #SpBrowseClassReferencesCommand,
	#superclass : #SpSystemSelectionCommand,
	#category : #'Spec2-Commands-Navigation'
}

{ #category : #default }
SpBrowseClassReferencesCommand class >> defaultDescription [
	^ 'Browse all references to the receiver''s class'
]

{ #category : #default }
SpBrowseClassReferencesCommand class >> shortName [

	^ 'class references'
]

{ #category : #execution }
SpBrowseClassReferencesCommand >> execute [

	self context doBrowseClassReferences
]
