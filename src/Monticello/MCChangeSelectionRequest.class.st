"
Notify about a change selection request
"
Class {
	#name : #MCChangeSelectionRequest,
	#superclass : #Notification,
	#instVars : [
		'patch',
		'label'
	],
	#category : 'Monticello-Versioning'
}

{ #category : #accessing }
MCChangeSelectionRequest >> label [
	^ label
]

{ #category : #accessing }
MCChangeSelectionRequest >> label: aString [
	label := aString
]

{ #category : #accessing }
MCChangeSelectionRequest >> patch [
	^ patch
]

{ #category : #accessing }
MCChangeSelectionRequest >> patch: aPatch [
	patch := aPatch
]
