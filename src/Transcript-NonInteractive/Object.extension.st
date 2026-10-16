Extension { #name : 'Object' }

{ #category : '*Transcript-NonInteractive' }
Object >> crTrace [
	"To deprecate in favor of #newLineTrace."

	self newLineTrace
]

{ #category : '*Transcript-NonInteractive' }
Object >> crTrace: aString [
	"To deprecate in favor of #newLineTrace:."

	self newLineTrace: aString
]

{ #category : '*Transcript-NonInteractive' }
Object >> logEntry [
	self traceCr: 'Entered ' , thisContext sender printString
]

{ #category : '*Transcript-NonInteractive' }
Object >> logExecution [
	self traceCr: 'Executing ' , thisContext sender printString
]

{ #category : '*Transcript-NonInteractive' }
Object >> logExit [
	self traceCr: 'Exited ' , thisContext sender printString
]

{ #category : '*Transcript-NonInteractive' }
Object >> newLineTrace [

	self newLineTrace: self
]

{ #category : '*Transcript-NonInteractive' }
Object >> newLineTrace: aString [
	"Log the argument. Use self newLineTrace: instead of Transcript newLine; show: "

	Transcript newLine.
	self trace: aString
]

{ #category : '*Transcript-NonInteractive' }
Object >> trace [
	self trace: self
]

{ #category : '*Transcript-NonInteractive' }
Object >> trace: anObject [
	"Log the argument. Use self trace: instead of Transcript show: "

	Transcript show: anObject asString
]

{ #category : '*Transcript-NonInteractive' }
Object >> traceCr [
	"To deprecate in favor of #traceNewLine."

	self traceNewLine
]

{ #category : '*Transcript-NonInteractive' }
Object >> traceCr: anObject [
	"To deprecate in favor of #traceNewLine:."

	self traceNewLine: anObject
]

{ #category : '*Transcript-NonInteractive' }
Object >> traceCrTab: anObject [
	"To deprecate in favor of #traceNewLineTab:"

	self traceNewLineTab: anObject
]

{ #category : '*Transcript-NonInteractive' }
Object >> traceNewLine [

	self traceNewLine: self
]

{ #category : '*Transcript-NonInteractive' }
Object >> traceNewLine: anObject [
	"Log the argument. Use self traceNewLine: 'something' instead of Transcript show: 'something'  ; newLine "

	self trace: anObject.
	Transcript newLine
]

{ #category : '*Transcript-NonInteractive' }
Object >> traceNewLineTab: anObject [
	"Log the argument. Use self traceNewLine: 'something' instead of Transcript show: 'something'  ; newLine "

	self trace: anObject.
	Transcript
		newLine;
		tab
]
