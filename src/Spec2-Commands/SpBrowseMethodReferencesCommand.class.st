"
I browse all references to the computed context that is transformed to a symbol.
"
Class {
	#name : #SpBrowseMethodReferencesCommand,
	#superclass : #SpSystemNavigationCommand,
	#category : #'Spec2-Commands-Navigation'
}

{ #category : #default }
SpBrowseMethodReferencesCommand class >> defaultDescription [
	^ 'Browse all references to the selected method or selector'
]

{ #category : #default }
SpBrowseMethodReferencesCommand class >> shortName [

	^ 'references'
]

{ #category : #executing }
SpBrowseMethodReferencesCommand >> execute [

	self systemNavigation browseAllReferencesTo: self selector
]
