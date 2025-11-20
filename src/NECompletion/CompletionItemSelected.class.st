"
A simple anouncement is announced when an item in the menu list is selected.
"
Class {
	#name : 'CompletionItemSelected',
	#superclass : 'Announcement',
	#instVars : [
		'item',
		'index',
		'entries',
		'token'
	],
	#category : 'NECompletion-Model',
	#package : 'NECompletion',
	#tag : 'Model'
}

{ #category : 'accessing' }
CompletionItemSelected >> entries [

	^ entries
]

{ #category : 'accessing' }
CompletionItemSelected >> entries: anObject [

	entries := anObject
]

{ #category : 'accessing' }
CompletionItemSelected >> index [

	^ index
]

{ #category : 'accessing' }
CompletionItemSelected >> index: aNumber [

	index := aNumber
]

{ #category : 'accessing' }
CompletionItemSelected >> selectedItem [

	^ item
]

{ #category : 'accessing' }
CompletionItemSelected >> selectedItem: anObject [

	item := anObject
]

{ #category : 'accessing' }
CompletionItemSelected >> token [

	^ token
]

{ #category : 'accessing' }
CompletionItemSelected >> token: anObject [

	token := anObject
]
