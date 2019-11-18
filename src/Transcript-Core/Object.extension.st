Extension { #name : #Object }

{ #category : #'*Transcript-Core' }
Object >> crTrace [
	self crTrace: self printString
]

{ #category : #'*Transcript-Core' }
Object >> crTrace: aString [
	"Log the argument. Use self crTrace: instead of Transcript cr; show: "
	
	Transcript cr; show: aString.
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
	self trace: self printString
]

{ #category : #'*Transcript-Core' }
Object >> trace: aString [
	"Log the argument. Use self trace: instead of Transcript show: "
	
	Transcript show: aString
]

{ #category : #'*Transcript-Core' }
Object >> traceCr [
	self traceCr: self printString
]

{ #category : #'*Transcript-Core' }
Object >> traceCr: aString [
	"Log the argument. Use self traceCr: 'something' instead of Transcript show: 'something'  ; cr "
	
	Transcript show: aString ; cr
]

{ #category : #'*Transcript-Core' }
Object >> traceCrTab: aString [
	"Log the argument. Use self traceCr: 'something' instead of Transcript show: 'something'  ; cr "
	
	Transcript show: aString ; cr ; tab
]
