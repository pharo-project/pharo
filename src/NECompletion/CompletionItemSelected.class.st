"
A simple anouncement is announced when an item in the menu list is selected.
"
Class {
	#name : 'CompletionItemSelected',
	#superclass : 'Announcement',
	#instVars : [
		'item',
		'index'
	],
	#category : 'NECompletion-Model',
	#package : 'NECompletion',
	#tag : 'Model'
}

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
