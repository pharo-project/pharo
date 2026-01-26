Class {
	#name : 'NECSnippetEntry',
	#superclass : 'NECEntry',
	#instVars : [
		'shortString'
	],
	#category : 'NECompletion-Model',
	#package : 'NECompletion',
	#tag : 'Model'
}

{ #category : 'private' }
NECSnippetEntry >> browse [
	self
]

{ #category : 'accessing' }
NECSnippetEntry >> createDescription [
	
	^ NECEntryDescription 
		label: 'Snippet replacement' title: shortString , ' into ' description: self contents
]

{ #category : 'accessing' }
NECSnippetEntry >> rawLabel [
	^ 'snippet'
]

{ #category : 'as yet unclassified' }
NECSnippetEntry >> shortString: aString [ 
	
	shortString := aString
]
