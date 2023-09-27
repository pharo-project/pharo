"
I'm announced in case when CodeImporter is about to evaluate a DoIt
"
Class {
	#name : 'DoItChunkImported',
	#superclass : 'SystemAnnouncement',
	#instVars : [
		'contents',
		'logSource'
	],
	#category : 'CodeImport-Utilities',
	#package : 'CodeImport',
	#tag : 'Utilities'
}

{ #category : 'accessing' }
DoItChunkImported >> contents [
	^ contents
]

{ #category : 'accessing' }
DoItChunkImported >> contents: anObject [
	contents := anObject
]

{ #category : 'accessing' }
DoItChunkImported >> logSource [
	^ logSource
]

{ #category : 'accessing' }
DoItChunkImported >> logSource: anObject [
	logSource := anObject
]
