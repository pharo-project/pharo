"
I browse a class that is returned from the evaluation of my context.
My context must be a block that returns a class.
"
Class {
	#name : #SpBrowseClassCommand,
	#superclass : #SpSystemSelectionCommand,
	#category : #'Spec2-Commands-Navigation'
}

{ #category : #default }
SpBrowseClassCommand class >> defaultDescription [
	^ 'Browse the selected class'
]

{ #category : #default }
SpBrowseClassCommand class >> defaultShortcutKey [

	^ $b command mac 
		| $b control win
		| $b control unix
]

{ #category : #default }
SpBrowseClassCommand class >> shortName [

	^ 'class'
]

{ #category : #execution }
SpBrowseClassCommand >> execute [
	| result |

	result := self 
		evaluate: self selectedTextOrLine 
		ifError: [ :e | ^ self ].
	self systemNavigation browse: result
]
