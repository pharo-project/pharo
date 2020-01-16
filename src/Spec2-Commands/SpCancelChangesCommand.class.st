Class {
	#name : #SpCancelChangesCommand,
	#superclass : #SpCodeCommand,
	#category : #'Spec2-Commands-Code'
}

{ #category : #defaults }
SpCancelChangesCommand class >> defaultIconName [
	^#smallCancel
]

{ #category : #default }
SpCancelChangesCommand class >> defaultName [

	^ 'Cancel'
]

{ #category : #execution }
SpCancelChangesCommand >> execute [
	self flag: 'Suspicious: is there not an more obvious/clean way of doing that?'.
	self context adapter widget cancel
]
