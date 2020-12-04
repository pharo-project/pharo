Class {
	#name : #SpCancelChangesCommand,
	#superclass : #SpToolCommand,
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
	
	self context triggerResetAction
]
