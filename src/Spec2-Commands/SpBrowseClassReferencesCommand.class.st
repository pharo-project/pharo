"
Browse all references to the class returned by the evaluation of the context.
"
Class {
	#name : #SpBrowseClassReferencesCommand,
	#superclass : #SpSystemNavigationCommand,
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
	| class |
	class := self context instanceSide.
	class isTrait
		ifTrue: [ self systemNavigation browseAllUsersOfTrait: class ]
		ifFalse: [ self systemNavigation browseAllCallsOnClass: class ]
]
