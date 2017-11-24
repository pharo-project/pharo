"
self openOn: 42
"
Class {
	#name : #GLMBrowserWithoutBlocksExample,
	#superclass : #GLMCompositePresentation,
	#category : #'Glamour-Examples'
}

{ #category : #'as yet unclassified' }
GLMBrowserWithoutBlocksExample >> compose [
	| wrapper |
	wrapper := self wrapper.
	wrapper show: [ :a |
		a list 
			display: (MessageSend receiver: self selector: #displayList:) ]
]

{ #category : #'as yet unclassified' }
GLMBrowserWithoutBlocksExample >> displayList: aNumber [
	^ 1 to: aNumber
]
