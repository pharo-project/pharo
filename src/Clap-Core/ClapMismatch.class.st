"
I am a failed match of my specification at a given word of the arguments sequence.
"
Class {
	#name : #ClapMismatch,
	#superclass : #ClapWordMatch,
	#category : #'Clap-Core-Activation'
}

{ #category : #accessing }
ClapMismatch >> at: aSpec [
	^ ClapImplicit of: aSpec in: self
]

{ #category : #matching }
ClapMismatch >> completeMatchOn: aStream [
	word:= aStream peek.
	startIndex := aStream position + 1.
]

{ #category : #testing }
ClapMismatch >> ifMatch: matchingBlock ifMismatch: mismatchBlock [
	^ mismatchBlock cull: self
]

{ #category : #testing }
ClapMismatch >> isValid [
	^ false
]

{ #category : #printing }
ClapMismatch >> printDetailsOn: aStream [
	aStream
		nextPutAll: word;
		nextPutAll: ' ≠ ';
		print: specification
]

{ #category : #adding }
ClapMismatch >> recordIn: parentMatch [ 
	"this match failed, so do nothing"
]
