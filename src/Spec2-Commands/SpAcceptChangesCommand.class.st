Class {
	#name : #SpAcceptChangesCommand,
	#superclass : #SpToolCommand,
	#category : #'Spec2-Commands-Code'
}

{ #category : #defaults }
SpAcceptChangesCommand class >> defaultIconName [
	^#smallOk 
]

{ #category : #default }
SpAcceptChangesCommand class >> defaultName [

	^ 'Accept'
]

{ #category : #execution }
SpAcceptChangesCommand >> execute [

	self context triggerSubmitAction
]
