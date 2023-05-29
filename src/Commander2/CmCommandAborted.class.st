"
I am raised when the execution of a command has been aborted.
"
Class {
	#name : #CmCommandAborted,
	#superclass : #CmCommandError,
	#instVars : [
		'reason'
	],
	#category : #'Commander2-Errors'
}

{ #category : #initialization }
CmCommandAborted >> initialize [
	super initialize.
	self
		reason: 'Unkown reason'
]

{ #category : #accessing }
CmCommandAborted >> reason [
	^ reason
]

{ #category : #accessing }
CmCommandAborted >> reason: anObject [
	reason := anObject
]
