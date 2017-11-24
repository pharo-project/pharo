"
GLMSmalltalkEditor is a helper class for Glamour. The only goal for this class is to provide the hook needed to let the morph know when the selection has changed.
"
Class {
	#name : #GLMSmalltalkEditor,
	#superclass : #SmalltalkEditor,
	#category : #'Glamour-Morphic-Widgets'
}

{ #category : #'class initialization' }
GLMSmalltalkEditor class >> initialize [
	"Initialize the keyboard shortcut maps and the shared buffers
	for copying text across views and managing again and undo." 
	super initialize
]

{ #category : #'new selection' }
GLMSmalltalkEditor >> storeSelectionInParagraph [
	super storeSelectionInParagraph.
	self morph ifNotNil: [
		self morph editView 
			announceSelection: self selection
			from: self startIndex 
			to: self stopIndex - 1 ]
]
