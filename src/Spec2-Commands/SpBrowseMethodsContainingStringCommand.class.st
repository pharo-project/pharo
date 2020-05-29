"
I browse all methods containing a string returned by the computed context.
"
Class {
	#name : #SpBrowseMethodsContainingStringCommand,
	#superclass : #SpSystemNavigationCommand,
	#category : #'Spec2-Commands-Navigation'
}

{ #category : #default }
SpBrowseMethodsContainingStringCommand class >> defaultDescription [
	^ 'Browse all methods containing the selected string'
]

{ #category : #default }
SpBrowseMethodsContainingStringCommand class >> shortName [

	^ 'methods containing string'
]

{ #category : #executing }
SpBrowseMethodsContainingStringCommand >> execute [

	self systemNavigation 
		browseMethodsWithSourceString: self target 
		matchCase: false
]
