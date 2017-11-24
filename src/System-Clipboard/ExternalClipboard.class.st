"
An ExternalClipboard is the abstract superclass for the platform specific clipboards based on the clipboard plugin (former ExtendedClipboardInterface originally developed for Sophie).

Instance Variables
	clipboard:		SmallInteger

clipboard
	- handle for the external clipboard. If 0 the external clipboard is invalid

"
Class {
	#name : #ExternalClipboard,
	#superclass : #Clipboard,
	#instVars : [
		'clipboard'
	],
	#category : #'System-Clipboard'
}

{ #category : #primitives }
ExternalClipboard >> addClipboardData: data dataFormat: aFormat [
	clipboard = 0 ifTrue: 
		[Clipboard clipboardText: data asString.	
		^self].
	self primAddClipboardData: clipboard data: data dataFormat: aFormat
]

{ #category : #accessing }
ExternalClipboard >> clearClipboard [
	clipboard = 0 ifTrue: [^self].
	^ self primClearClipboard: clipboard.
]

{ #category : #accessing }
ExternalClipboard >> clipboardText [
	"Return the text currently in the clipboard. If the system clipboard is empty, or if it differs from the Smalltalk clipboard text, use the Smalltalk clipboard. This is done since (a) the Mac clipboard gives up on very large chunks of text and (b) since not all platforms support the notion of a clipboard."

	| decodedString bytes |
	clipboard = 0 ifTrue: 
		[^super clipboardText].
	bytes := self primReadClipboardData: clipboard format: 'public.utf8-plain-text'.
	bytes
		ifNil: [^super clipboardText].
	decodedString := bytes asString convertFromWithConverter: UTF8TextConverter new.
	decodedString := decodedString replaceAll: 10 asCharacter with: 13 asCharacter.
	^decodedString = contents asString
		ifTrue: [contents]
		ifFalse: [decodedString asText].

]

{ #category : #accessing }
ExternalClipboard >> clipboardText: text [ 

	| string data |
	string := text asString.
	self noteRecentClipping: text asText copyWithoutExternalReferences.
	contents := text asText copyWithoutExternalReferences.
	data := (string convertToWithConverter: UTF8TextConverter new) asByteArray.
	clipboard = 0 ifTrue: 
		[^super clipboardText: text].
	self clearClipboard.
	self primAddClipboardData: clipboard data: data dataFormat: 'public.utf8-plain-text'
]

{ #category : #private }
ExternalClipboard >> createClipboard [
	clipboard = 0 ifTrue: [^self].
	^ self primCreateClipboard.
]

{ #category : #initialization }
ExternalClipboard >> initialize [
	super initialize.
	clipboard := [self createClipboard] on: Error do: [:ex | clipboard := 0]
]

{ #category : #primitives }
ExternalClipboard >> primAddClipboardData: aClipboard data: data dataFormat: aFormat [

	<primitive:'ioAddClipboardData' module: 'ClipboardExtendedPlugin'>
	^ self primitiveFailed
]

{ #category : #primitives }
ExternalClipboard >> primClearClipboard:  aClipboard [
	<primitive:'ioClearClipboard' module: 'ClipboardExtendedPlugin'>
	^ self primitiveFailed.

]

{ #category : #primitives }
ExternalClipboard >> primCreateClipboard [
	<primitive:'ioCreateClipboard' module: 'ClipboardExtendedPlugin'>
	^ self primitiveFailed.

]

{ #category : #primitives }
ExternalClipboard >> primGetClipboardFormat: aClipboard formatNumber: formatNumber [

	<primitive:'ioGetClipboardFormat' module: 'ClipboardExtendedPlugin'>
	^ self primitiveFailed
]

{ #category : #primitives }
ExternalClipboard >> primReadClipboardData: aClipboard format: format [

	<primitive:'ioReadClipboardData' module: 'ClipboardExtendedPlugin'>
	^ self primitiveFailed
]
