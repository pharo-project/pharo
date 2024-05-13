"
I implement the entry point of the clipboard to the system.

One of my instance is used as a default null implementation, storing the copied values in an instance variable.

A real OSWindowClipboard will replace me, but I am still in case something is using the clipboard when there is no real OSWindow.
"
Class {
	#name : 'Clipboard',
	#superclass : 'Object',
	#instVars : [
		'contents',
		'recentClippings'
	],
	#classVars : [
		'Default'
	],
	#category : 'System-Utilities-Base',
	#package : 'System-Utilities',
	#tag : 'Base'
}

{ #category : 'accessing' }
Clipboard class >> clipboardText [
 	<script: 'self clipboardText inspect'>

	^ self default clipboardText
]

{ #category : 'accessing' }
Clipboard class >> clipboardText: aTextOrString [

	^ self default clipboardText: aTextOrString
]

{ #category : 'accessing' }
Clipboard class >> default [

	^ Default ifNil: [ Default := self new ]
]

{ #category : 'testing' }
Clipboard class >> hasRecentClippings [
 	<script: 'self hasRecentClippings inspect'>

	^ self default hasRecentClippings
]

{ #category : 'class initialization' }
Clipboard class >> initialize [

	self startUp: true.
	SessionManager default registerSystemClassNamed: self name
]

{ #category : 'accessing' }
Clipboard class >> recentClippings [
	"Return a collection of the last remembered clippings in LIFO order."

	^ self default recentClippings
]

{ #category : 'class initialization' }
Clipboard class >> reset [
	<script>

	Default := nil
]

{ #category : 'system startup' }
Clipboard class >> shutDown: isImageQuitting [
	"Pharo is shutting down. If this platform requires specific shutdown code, this is a great place to put it."
]

{ #category : 'system startup' }
Clipboard class >> startUp: isImageStarting [
	"Pharo is starting up. If this platform requires specific initialization, this is a great place to put it."

	isImageStarting ifTrue: [ self reset ]
]

{ #category : 'accessing' }
Clipboard >> clipboardText [

	^ contents
]

{ #category : 'accessing' }
Clipboard >> clipboardText: text [

	self noteRecentClipping: text asText copyWithoutExternalReferences.
	contents := text asText copyWithoutExternalReferences
]

{ #category : 'testing' }
Clipboard >> hasRecentClippings [

	^ self recentClippings notEmpty
]

{ #category : 'initialization' }
Clipboard >> initialize [
	super initialize.
	contents := '' asText
]

{ #category : 'private' }
Clipboard >> noteRecentClipping: text [
	"Keep most recent clippings - if not too large - in a queue to allow to paste them again
 	 (like with a 'paste...' command or other)"

	text isEmpty ifTrue: [ ^ self ].
	text size > 50000 ifTrue: [ ^ self ].
	(self recentClippings includes: text) ifTrue: [ ^ self ].
	self recentClippings addFirst: text.
	[ self recentClippings size > 5 ] whileTrue: [ self recentClippings removeLast ]
]

{ #category : 'accessing' }
Clipboard >> recentClippings [
	"Return a collection of the last remembered clippings in LIFO order."

	^ recentClippings ifNil: [ recentClippings := OrderedCollection new ]
]
