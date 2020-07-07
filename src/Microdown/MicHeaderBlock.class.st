"
I am a header. I look like this:

# level 1
### level 3

Deepest level is level 6
"
Class {
	#name : #MicHeaderBlock,
	#superclass : #MicSingleLineBlock,
	#instVars : [
		'level',
		'header'
	],
	#category : #'Microdown-Model'
}

{ #category : #testing }
MicHeaderBlock >> addLineAndReturnNextNode: line [
	"Line is on the form '#### my heading at forth level'"
	level := ('(#+)' asRegex
		matchesPrefix: line;
		subexpression: 1) size.
	header := (line copyFrom: level + 1 to: line size) trim.
	^ self
]

{ #category : #public }
MicHeaderBlock >> contents [ 
	^ super contents , '[ ', self level printString, '/', self header, ' ]'
]

{ #category : #accessing }
MicHeaderBlock >> formattedCode [ 
	^super formattedCode , '[ ', self level printString, '/', self header, ' ]'
]

{ #category : #accessing }
MicHeaderBlock >> header [
	^ header
]

{ #category : #accessing }
MicHeaderBlock >> level [
	^ level
]

{ #category : #printing }
MicHeaderBlock >> printOn: aStream [

	aStream nextPutAll: 'Section: '.
	aStream nextPutAll: header
]
