"
I represent the conditions to stop the scan
"
Class {
	#name : #RubTextStopConditions,
	#superclass : #Object,
	#instVars : [
		'stops',
		'crossedX',
		'endOfRun'
	],
	#category : #'Rubric-TextScanning'
}

{ #category : #accessing }
RubTextStopConditions >> at: anInteger [
	"Answer the special action associated with a character of code anInteger, or nil if none.
	The character code should be betxween 1 and 256 (presumably codePoint + 1).
	It can eventually be 257 for endOfRun action, or 258 for crossedX action for backward compatibility with historical squeak versions."
	
	^stops at: anInteger
]

{ #category : #accessing }
RubTextStopConditions >> at: anInteger put: aSymbolOrNil [
	"Set the special action associated with a character of code anInteger, or nil if none.
	The character code should be betxween 1 and 256 (presumably codePoint + 1).
	It can eventually be 257 for endOfRun action, or 258 for crossedX action for backward compatibility with historical Squeak versions."
	
	anInteger = 257
		ifTrue: [
			self endOfRun: aSymbolOrNil.
			^aSymbolOrNil].
	anInteger = 258
		ifTrue: [
			self crossedX: aSymbolOrNil.
			^aSymbolOrNil].
	^stops at: anInteger put: aSymbolOrNil
]

{ #category : #accessing }
RubTextStopConditions >> crossedX [
	"Answer the special action to be performed when crossing composition bounds."
	
	^crossedX
]

{ #category : #accessing }
RubTextStopConditions >> crossedX: aSymbolOrNil [
	crossedX := aSymbolOrNil.
	
	"Backward compatibility with historical EndOfRun TextConstant handling"
	stops size >= 258 ifTrue: [stops at: 258 put: crossedX]
]

{ #category : #accessing }
RubTextStopConditions >> endOfRun [
	"Answer the special action to be performed et end of text."
	
	^endOfRun
]

{ #category : #accessing }
RubTextStopConditions >> endOfRun: aSymbolOrNil [
	endOfRun := aSymbolOrNil.
	
	"Backward compatibility with historical EndOfRun TextConstant handling"
	stops size >= 257 ifTrue: [stops at: 257 put: endOfRun]
]

{ #category : #'initialize-release' }
RubTextStopConditions >> initialize [
	"Initialize the default stop conditions."
	stops := Array new: 258.
	self endOfRun: #endOfRun.
	self crossedX: #crossedX.
]

{ #category : #copying }
RubTextStopConditions >> postCopy [
	stops := stops copy
]

{ #category : #'initialize-release' }
RubTextStopConditions >> setStops: stopArray [
	stops := stopArray
]

{ #category : #accessing }
RubTextStopConditions >> size [
	"Return the highest code to which an action can be mapped.
	Do not take into account the two slots for endOfRun and crossedX, since they don't map any character code.
	Warning: this message is used for compatibility with obsolete code."
	
	^256
]
