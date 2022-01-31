"
I implement the entry point of the clipboard to the system.

One of my instance is used as a default null implementation, storing the copied values in an instance variable.

A real OSWindowClipboard will replace me, but I am still in case something is using the clipboard when there is no real OSWindow.
"
Class {
	#name : #Clipboard,
	#superclass : #Object,
	#instVars : [
		'contents',
		'recent'
	],
	#classVars : [
		'Default'
	],
	#category : #'Morphic-Core'
}

{ #category : #accessing }
Clipboard class >> chooseRecentClipping [  "Clipboard chooseRecentClipping"
	"Choose by menu from among the recent clippings"
	^self default chooseRecentClipping
]

{ #category : #accessing }
Clipboard class >> clipboardText [ "Clipboard clipboardText"
	^self default clipboardText.
]

{ #category : #accessing }
Clipboard class >> clipboardText: aText [ 
	^self default clipboardText: aText
]

{ #category : #accessing }
Clipboard class >> default [
	^Default ifNil: [Default := self new].
]

{ #category : #initialization }
Clipboard class >> initialize [
	self startUp: true.
	SessionManager default 
		registerSystemClassNamed: self name
]

{ #category : #'system startup' }
Clipboard class >> shutDown: isImageQuitting [
	"Pharo is shutting down. If this platform requires specific shutdown code, this is a great place to put it."
]

{ #category : #'system startup' }
Clipboard class >> startUp: isImageStarting [
	"Pharo is starting up. If this platform requires specific initialization, this is a great place to put it."
	isImageStarting
		ifTrue: [Default := nil]
]

{ #category : #accessing }
Clipboard >> chooseRecentClipping [

	"Choose by menu from among the recent clippings"

	"Clipboard chooseRecentClipping"

	recent ifNil: [ ^ nil ].
	^ UIManager default
		  chooseFrom: (recent collect: [ :txt | 
				   ((txt asString contractTo: 50)
					    copyReplaceAll: String cr
					    with: '\') copyReplaceAll: String tab with: '|' ])
		  values: recent
]

{ #category : #accessing }
Clipboard >> clipboardText [

	^ contents
]

{ #category : #accessing }
Clipboard >> clipboardText: text [

	self noteRecentClipping: text asText copyWithoutExternalReferences.
	contents := text asText copyWithoutExternalReferences.
]

{ #category : #initialization }
Clipboard >> initialize [
	super initialize.
	contents := '' asText.
	recent := OrderedCollection new
]

{ #category : #private }
Clipboard >> noteRecentClipping: text [
	"Keep most recent clippings in a queue for pasteRecent (paste... command)"
	text isEmpty ifTrue: [^ self].
	text size > 50000 ifTrue: [^ self].
	(recent includes: text) ifTrue: [^ self].
	recent addFirst: text.
	[recent size > 5] whileTrue: [recent removeLast].
]
