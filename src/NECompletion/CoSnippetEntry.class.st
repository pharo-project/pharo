Class {
	#name : 'CoSnippetEntry',
	#superclass : 'CoEntry',
	#instVars : [
		'shortString'
	],
	#category : 'NECompletion-Model',
	#package : 'NECompletion',
	#tag : 'Model'
}

{ #category : 'private' }
CoSnippetEntry >> browseCompletionItem [
	self
]

{ #category : 'accessing' }
CoSnippetEntry >> createDescription [
	
	^ CoEntryDescription 
		label: 'Snippet replacement' title: shortString , ' into ' description: self contents
]

{ #category : 'accessing' }
CoSnippetEntry >> rawLabel [
	^ 'snippet'
]

{ #category : 'private' }
CoSnippetEntry >> shortString: aString [ 
	
	shortString := aString
]
