Extension { #name : 'Object' }

{ #category : '*Transcript-NonInteractive' }
Object >> crTrace [
	self crTrace: self
]

{ #category : '*Transcript-NonInteractive' }
Object >> crTrace: aString [
	"Log the argument. Use self crTrace: instead of Transcript cr; show: "

	Transcript cr.
	self trace: aString
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
	self traceCr: self
]

{ #category : '*Transcript-NonInteractive' }
Object >> traceCr: anObject [
	"Log the argument. Use self traceCr: 'something' instead of Transcript show: 'something'  ; cr "

	self trace: anObject.
	Transcript cr 
]

{ #category : '*Transcript-NonInteractive' }
Object >> traceCrTab: anObject [
	"Log the argument. Use self traceCr: 'something' instead of Transcript show: 'something'  ; cr "

	self trace: anObject.
	Transcript cr ; tab
]
