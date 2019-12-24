Extension { #name : #Object }

{ #category : #'*Transcript-Core' }
Object >> crTrace [
	self crTrace: self
]

{ #category : #'*Transcript-Core' }
Object >> crTrace: aString [
	"Log the argument. Use self crTrace: instead of Transcript cr; show: "
	
	Transcript cr.
	self trace: aString
]

{ #category : #'*Transcript-Core' }
Object >> logEntry [
	self traceCr: 'Entered ' , thisContext sender printString
]

{ #category : #'*Transcript-Core' }
Object >> logExecution [
	self traceCr: 'Executing ' , thisContext sender printString
]

{ #category : #'*Transcript-Core' }
Object >> logExit [
	self traceCr: 'Exited ' , thisContext sender printString
]

{ #category : #'*Transcript-Core' }
Object >> trace [
	self trace: self
]

{ #category : #'*Transcript-Core' }
Object >> trace: anObject [
	"Log the argument. Use self trace: instead of Transcript show: "
	
	Transcript show: anObject asString
]

{ #category : #'*Transcript-Core' }
Object >> traceCr [
	self traceCr: self
]

{ #category : #'*Transcript-Core' }
Object >> traceCr: anObject [
	"Log the argument. Use self traceCr: 'something' instead of Transcript show: 'something'  ; cr "
	
	self trace: anObject.
	Transcript cr
]

{ #category : #'*Transcript-Core' }
Object >> traceCrTab: anObject [
	"Log the argument. Use self traceCr: 'something' instead of Transcript show: 'something'  ; cr "
	
	self trace: anObject.
	Transcript cr ; tab
]
