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
	| result class |

	result := self 
		evaluate: self selectedTextOrLine 
		ifError: [ :e | ^ self ].
		
	(result isClass or: [ result isTrait ])
		ifFalse: [ result := result class ].

	class := result instanceSide.
	class isTrait
		ifTrue: [ self systemNavigation browseAllUsersOfTrait: class ]
		ifFalse: [ self systemNavigation browseAllCallsOnClass: class ]
]
