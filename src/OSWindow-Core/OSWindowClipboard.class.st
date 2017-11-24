"
This is a private class for handling clipboard text. There's no direct use of it.
For setting/retrieving clipboard text, use clipboardText/clipboardText: accessors of OSWindow instances.
"
Class {
	#name : #OSWindowClipboard,
	#superclass : #Clipboard,
	#category : #'OSWindow-Core'
}

{ #category : #accessing }
OSWindowClipboard >> chooseRecentClipping [
	^ nil
]

{ #category : #accessing }
OSWindowClipboard >> clipboardText [
	^ World osWindow clipboardText
]

{ #category : #accessing }
OSWindowClipboard >> clipboardText: aString [
	World osWindow clipboardText: aString asString
]

{ #category : #private }
OSWindowClipboard >> noteRecentClipping: text [
]
