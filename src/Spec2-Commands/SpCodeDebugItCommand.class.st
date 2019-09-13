Class {
	#name : #SpCodeDebugItCommand,
	#superclass : #SpCodeSelectionCommand,
	#category : #'Spec2-Commands-Code'
}

{ #category : #default }
SpCodeDebugItCommand class >> defaultName [ 

	^ 'Debug it'
]

{ #category : #default }
SpCodeDebugItCommand class >> defaultShortcutKey [

	^ $d shift meta
]

{ #category : #execution }
SpCodeDebugItCommand >> execute [

	'OK' crLog
]
