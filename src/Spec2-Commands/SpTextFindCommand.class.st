Class {
	#name : #SpTextFindCommand,
	#superclass : #SpTextSearchCommand,
	#category : #'Spec2-Commands-Code'
}

{ #category : #default }
SpTextFindCommand class >> defaultIconName [

	^ #smallFind
]

{ #category : #defaults }
SpTextFindCommand class >> defaultShortcutKey [

	^ $f command mac 
		| $f control unix
		| $f control win
]

{ #category : #executing }
SpTextFindCommand >> execute [

	context doTextFind
]
