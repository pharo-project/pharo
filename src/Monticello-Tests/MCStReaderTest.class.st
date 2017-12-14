Class {
	#name : #MCStReaderTest,
	#superclass : #MCTestCase,
	#category : #'Monticello-Tests-IO'
}

{ #category : #util }
MCStReaderTest >> commentWithStyle [
	^ '!AEDesc commentStamp: ''<historical>'' prior: 0!
I represent an Apple Event Descriptor.  I am a low-level representation of Apple Event (and hence Applescript) information.  For further Information, see Apple''s Inside Macintosh: Interapplication Communications, at

	http://developer.apple.com/techpubs/mac/IAC/IAC-2.html.

Essentially, I represent a record comprising a one-word "string" (treating the word as fourbyte characters) representing a data type, followed by a pointer to a pointer (a handle) to the data I represent.  Care must be taken to assure that the Handle data is disposed after use, or memory leaks result.  At this time, I make no effort to do this automatically through finalization.!
]style[(218 54 384)f1,f1Rhttp://developer.apple.com/techpubs/mac/IAC/IAC-2.html;,f1!
'
]

{ #category : #util }
MCStReaderTest >> commentWithoutStyle [
	^ '
CharacterScanner subclass: #CanvasCharacterScanner
	instanceVariableNames: ''canvas fillBlt foregroundColor runX lineY ''
	classVariableNames: ''''
	poolDictionaries: ''''
	category: ''Morphic-Support''!

!CanvasCharacterScanner commentStamp: ''<historical>'' prior: 0!
A displaying scanner which draws its output to a Morphic canvas.!

!CanvasCharacterScanner methodsFor: ''stop conditions'' stamp: ''ar 12/15/2001 23:27''!
setStopConditions
	"Set the font and the stop conditions for the current run."

	self setFont.
	stopConditions
		at: Space asciiValue + 1
		put: (alignment = Justified ifTrue: [#paddedSpace])! !'
]

{ #category : #util }
MCStReaderTest >> methodWithStyle [
	^ '!EventHandler methodsFor: ''copying'' stamp: ''tk 1/22/2001 17:39''!
veryDeepInner: deepCopier
	"ALL fields are weakly copied.  Can''t duplicate an object by duplicating a button that activates it.  See DeepCopier."

	super veryDeepInner: deepCopier.
	"just keep old pointers to all fields"
	clickRecipient := clickRecipient.!
! !

'
]

{ #category : #tests }
MCStReaderTest >> testCommentWithStyle [
	| reader |
	reader := MCStReader on: self commentWithStyle readStream.
	reader definitions
]

{ #category : #tests }
MCStReaderTest >> testCommentWithoutStyle [
	| reader |
	reader := MCStReader on: self commentWithoutStyle readStream.
	self assert: (reader definitions anySatisfy: [:ea | ea isMethodDefinition]).
]

{ #category : #tests }
MCStReaderTest >> testMethodWithStyle [
	| reader |
	reader := MCStReader on: self methodWithStyle readStream.
	self assert: reader definitions first isMethodDefinition.
]
