"
A simple anouncement is announced when an item in the menu list is selected.
"
Class {
	#name : 'CoCompletionItemSelected',
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
CoCompletionItemSelected >> entries [

	^ entries
]

{ #category : 'accessing' }
CoCompletionItemSelected >> entries: anObject [

	entries := anObject
]

{ #category : 'accessing' }
CoCompletionItemSelected >> index [

	^ index
]

{ #category : 'accessing' }
CoCompletionItemSelected >> index: aNumber [

	index := aNumber
]

{ #category : 'accessing' }
CoCompletionItemSelected >> selectedItem [

	^ item
]

{ #category : 'accessing' }
CoCompletionItemSelected >> selectedItem: anObject [

	item := anObject
]

{ #category : 'accessing' }
CoCompletionItemSelected >> token [

	^ token
]

{ #category : 'accessing' }
CoCompletionItemSelected >> token: anObject [

	token := anObject
]
