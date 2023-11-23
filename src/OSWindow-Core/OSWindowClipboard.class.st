"
This is a private class for handling clipboard text. There's no direct use of it.
For setting/retrieving clipboard text, use clipboardText/clipboardText: accessors of OSWindow instances.
"
Class {
	#name : 'OSWindowClipboard',
	#superclass : 'Clipboard',
	#category : 'OSWindow-Core-Base',
	#package : 'OSWindow-Core',
	#tag : 'Base'
}

{ #category : 'configurations' }
OSWindowClipboard >> beDefault [

	Default := self
]

{ #category : 'accessing' }
OSWindowClipboard >> clipboardText [
	^ self worldRenderer clipboardText withInternalLineEndings
]

{ #category : 'accessing' }
OSWindowClipboard >> clipboardText: aString [
	self worldRenderer clipboardText: aString asString.
	self noteRecentClipping: aString
]

{ #category : 'private - accessing' }
OSWindowClipboard >> worldRenderer [

	^ self currentWorld worldState worldRenderer
]
