"
I'm announced in case when CodeImporter is about to evaluate a DoIt
"
Class {
	#name : #DoItChunkImported,
	#superclass : #SystemAnnouncement,
	#instVars : [
		'content',
		'requestor',
		'logSource'
	],
	#category : #'CodeImport-Utilities'
}

{ #category : #accessing }
DoItChunkImported >> content [
	^ content
]

{ #category : #accessing }
DoItChunkImported >> content: anObject [
	content := anObject
]

{ #category : #accessing }
DoItChunkImported >> logSource [
	^ logSource
]

{ #category : #accessing }
DoItChunkImported >> logSource: anObject [
	logSource := anObject
]

{ #category : #accessing }
DoItChunkImported >> requestor [
	^ requestor
]

{ #category : #accessing }
DoItChunkImported >> requestor: anObject [
	requestor := anObject
]
