"
Notify to resolve conflicts by merging
"
Class {
	#name : #MCMergeResolutionRequest,
	#superclass : #Notification,
	#instVars : [
		'merger'
	],
	#category : #'Monticello-Versioning'
}

{ #category : #actions }
MCMergeResolutionRequest >> autoMerge [
	"If there are not conflicts, merge, otherwise delegate to the UI"
	self hasConflicts
		ifTrue: [ self resolve ]
		ifFalse: [ self merge ]
]

{ #category : #testing }
MCMergeResolutionRequest >> hasConflicts [
	^ self merger hasConflicts
]

{ #category : #actions }
MCMergeResolutionRequest >> merge [
	self resume: true
]

{ #category : #accessing }
MCMergeResolutionRequest >> merger [
	^ merger
]

{ #category : #accessing }
MCMergeResolutionRequest >> merger: aMerger [
	merger := aMerger
]

{ #category : #actions }
MCMergeResolutionRequest >> resolve [
	self pass
]
