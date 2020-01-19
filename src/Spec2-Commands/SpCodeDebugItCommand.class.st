Class {
	#name : #SpCodeDebugItCommand,
	#superclass : #SpCodeSelectionCommand,
	#category : #'Spec2-Commands-Code'
}

{ #category : #defaults }
SpCodeDebugItCommand class >> defaultIconName [
	^#smallDebug
]

{ #category : #default }
SpCodeDebugItCommand class >> defaultName [ 

	^ 'Debug it'
]

{ #category : #default }
SpCodeDebugItCommand class >> defaultShortcutKey [

	^ $d shift command mac 
		| $d shift control win
		| $d shift control unix
]

{ #category : #execution }
SpCodeDebugItCommand >> execute [

	'OK' crLog
]
