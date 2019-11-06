"
I'm announced in case when CodeImporter is about to evaluate a DoIt
"
Class {
	#name : #DoItChunkImported,
	#superclass : #SystemAnnouncement,
	#instVars : [
		'contents',
		'requestor',
		'logSource'
	],
	#category : #'CodeImport-Utilities'
}

{ #category : #accessing }
DoItChunkImported >> contents [
	^ contents
]

{ #category : #accessing }
DoItChunkImported >> contents: anObject [
	contents := anObject
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
