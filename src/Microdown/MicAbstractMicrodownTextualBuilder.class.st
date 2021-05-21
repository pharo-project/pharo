"
I'm a copy of Pillar textual canvas. I should be fusioned in my subclass.
We should revisit the API because parsing lines is a not really good design.
"
Class {
	#name : #MicAbstractMicrodownTextualBuilder,
	#superclass : #Object,
	#instVars : [
		'stream',
		'parserClass',
		'lastIsNewLine'
	],
	#category : #'Microdown-Parser'
}

{ #category : #testing }
MicAbstractMicrodownTextualBuilder class >> isAbstract [

	^ self == MicAbstractMicrodownTextualBuilder
]

{ #category : #'instance creation' }
MicAbstractMicrodownTextualBuilder class >> on: aStream [ 
	^ self new
		setStream: aStream;
		yourself
]

{ #category : #formatting }
MicAbstractMicrodownTextualBuilder >> bold: aText [

	self writeText: aText surroundBy: parserClass boldMarkup
]

{ #category : #accessing }
MicAbstractMicrodownTextualBuilder >> contents [
	^ stream contents
]

{ #category : #initialization }
MicAbstractMicrodownTextualBuilder >> flush [
	stream flush
]

{ #category : #initialization }
MicAbstractMicrodownTextualBuilder >> initialize [
	super initialize.
	stream := (String new: 1000) writeStream.
	parserClass := MicroDownParser.
	lastIsNewLine := true
]

{ #category : #formatting }
MicAbstractMicrodownTextualBuilder >> italic: aText [

	self writeText: aText surroundBy: parserClass italicMarkup
]

{ #category : #testing }
MicAbstractMicrodownTextualBuilder >> lastIsNewLine [
	^ lastIsNewLine
]

{ #category : #'writing text' }
MicAbstractMicrodownTextualBuilder >> line: aString [
	self
		nextPutAll: aString;
		newLine
]

{ #category : #formatting }
MicAbstractMicrodownTextualBuilder >> monospace: aText [

	self writeText: aText surroundBy: parserClass monospaceMarkup
]

{ #category : #'writing text' }
MicAbstractMicrodownTextualBuilder >> newLine [
	self raw: String cr.
	lastIsNewLine := true
]

{ #category : #'writing text' }
MicAbstractMicrodownTextualBuilder >> nextPut: aCharacter [
	stream nextPut: aCharacter.
	lastIsNewLine := false
]

{ #category : #'writing text' }
MicAbstractMicrodownTextualBuilder >> nextPutAll: aString [
	
	aString do: [ :char | self nextPut: char ]
]

{ #category : #'writing low-level' }
MicAbstractMicrodownTextualBuilder >> potentialNewLine [
	lastIsNewLine ifFalse: [ self newLine ]
]

{ #category : #'writing text' }
MicAbstractMicrodownTextualBuilder >> raw: aString [
	stream << aString
]

{ #category : #initialization }
MicAbstractMicrodownTextualBuilder >> setStream: aStream [ 
	stream := aStream
]

{ #category : #'writing text' }
MicAbstractMicrodownTextualBuilder >> space [
	stream space
]

{ #category : #formatting }
MicAbstractMicrodownTextualBuilder >> strike: aText [

	self writeText: aText surroundBy: parserClass strikeMarkup
]

{ #category : #writing }
MicAbstractMicrodownTextualBuilder >> text: aText [
	self raw: aText
	
]

{ #category : #writing }
MicAbstractMicrodownTextualBuilder >> writeLinkDuring: aBlockClosure destination: aString [ 
	
	self
		writeLinkDuring: aBlockClosure
		destination: aString
		title: nil
]

{ #category : #writing }
MicAbstractMicrodownTextualBuilder >> writeLinkDuring: aBlockClosure destination: aDestination title: aTitle [ 

	self raw: '['.
	aBlockClosure ensure: [ 
		self
			raw: '](';
			raw: aDestination.
		
		aTitle ifNotNil: [ 
			self
				raw: ' "';
				raw: aTitle;
				raw: '"'.
		].
		self raw: ')'
	 ]
]

{ #category : #writing }
MicAbstractMicrodownTextualBuilder >> writeNewLine [
	self raw: String cr
	
]

{ #category : #writing }
MicAbstractMicrodownTextualBuilder >> writeText: aText beginsWith: aMarkup [
	self
		raw: aMarkup;
		raw: String space;
		raw: aText
]

{ #category : #writing }
MicAbstractMicrodownTextualBuilder >> writeText: aText surroundBy: aMarkup [
	self writeText: aText surroundBy: aMarkup and: aMarkup
]

{ #category : #writing }
MicAbstractMicrodownTextualBuilder >> writeText: aText surroundBy: anOpenerMarkup and: aCloserMarkup [
	self
		raw: anOpenerMarkup;
		raw: aText;
		raw: aCloserMarkup
]
