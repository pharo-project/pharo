"
The Clipboard class is the abstract superclass for the concrete platform specific clipboard. The legacy clipboard support using the VM supplied primitives is implemented by Clipboard/ExternalClipboard.

The Clipboard implements a basic buffering scheme for text. The currently selected text is also exported to the OS so that text can be copied from and to other applications. Commonly only a single instance is used (the default clipboard) but applications are free to use other than the default clipboard if necessary.
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
	#category : 'System-Clipboard'
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
	^Default ifNil: [Default := Smalltalk os clipboardClass new].
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
	"Pharo is starting up. If this platform requires specific intialization, this is a great place to put it."
	isImageStarting
		ifTrue: [Default := nil]
]

{ #category : #accessing }
Clipboard >> chooseRecentClipping [  
	"Choose by menu from among the recent clippings"
	"Clipboard chooseRecentClipping"
	
	recent ifNil: [^ nil].
	^ UIManager default
		chooseFrom: (recent collect: [:txt | ((txt asString contractTo: 50)
									copyReplaceAll: Character cr asString with: '\')
									copyReplaceAll: Character tab asString with: '|'])
		values: recent
]

{ #category : #accessing }
Clipboard >> clipboardText [
	"Return the text currently in the clipboard. If the system clipboard is empty, or if it differs from the Smalltalk clipboard text, use the Smalltalk clipboard. This is done since (a) the Mac clipboard gives up on very large chunks of text and (b) since not all platforms support the notion of a clipboard."

	| string decodedString |
	string := self primitiveClipboardText.
	(string isEmpty
			or: [string = contents asString])
		ifTrue: [^ contents].
	decodedString := string convertFromWithConverter: UTF8TextConverter new.
	decodedString := decodedString replaceAll: 10 asCharacter with: 13 asCharacter.
	^ decodedString = contents asString
		ifTrue: [contents]
		ifFalse: [decodedString asText].

]

{ #category : #accessing }
Clipboard >> clipboardText: text [ 

        | string |
        string := text asString.
        self noteRecentClipping: text asText copyWithoutExternalReferences.
        contents := text asText copyWithoutExternalReferences.
        string := string convertToWithConverter: UTF8TextConverter new.
        self primitiveClipboardText: string.
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

{ #category : #primitives }
Clipboard >> primitiveClipboardText [
	"Get the current clipboard text. Return the empty string if the primitive fails."
	<primitive: 141>
	^ ''
]

{ #category : #primitives }
Clipboard >> primitiveClipboardText: aString [
	"Set the current clipboard text to the given string."

	<primitive: 141>
	"don't fail if the primitive is not implemented"
]
