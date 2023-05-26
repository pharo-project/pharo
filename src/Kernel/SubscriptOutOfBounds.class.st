"
I am SubscriptOutOfBounds, an exception indicating that some operation attempted to use a subscript outside allowed bounds.

Normally, I hold the offending subscript and/or the allowed lowerBound and upperBound (inclusive).

SubscriptOutOfBounds 
	signalFor: 10 
	lowerBound: 1 
	upperBound: 5 
	in: (Array new: 5)
"
Class {
	#name : #SubscriptOutOfBounds,
	#superclass : #Error,
	#instVars : [
		'subscript',
		'lowerBound',
		'upperBound'
	],
	#category : #'Kernel-Exceptions'
}

{ #category : #signaling }
SubscriptOutOfBounds class >> signalFor: subscript [
	^ self
		signalFor: subscript
		lowerBound: nil
		upperBound: nil
]

{ #category : #signaling }
SubscriptOutOfBounds class >> signalFor: subscript lowerBound: lowerBound upperBound: upperBound [
	^ self
		signalFor: subscript
		lowerBound: lowerBound
		upperBound: upperBound
		in: nil
]

{ #category : #signaling }
SubscriptOutOfBounds class >> signalFor: subscript lowerBound: lowerBound upperBound: upperBound in: object [
	^ self new
		signaler: object;
		subscript: subscript;
		lowerBound: lowerBound;
		upperBound: upperBound;
		signal
]

{ #category : #accessing }
SubscriptOutOfBounds >> lowerBound [
	^ lowerBound
]

{ #category : #accessing }
SubscriptOutOfBounds >> lowerBound: anObject [
	lowerBound := anObject
]

{ #category : #accessing }
SubscriptOutOfBounds >> messageText [
	"Overwritten to initialiaze the message text to a standard text if it has not yet been set"

	^ messageText ifNil: [ messageText := self standardMessageText ]
]

{ #category : #printing }
SubscriptOutOfBounds >> standardMessageText [
	"Generate a standard textual description"

	^ String streamContents: [ :stream |
		  self subscript
			  ifNil: [ stream << 'subscript' ]
			  ifNotNil: [ stream print: self subscript ].
		  (self lowerBound notNil and: [ self upperBound notNil ]) ifTrue: [
			  stream
				  << ' is not between ';
				  print: self lowerBound;
				  << ' and ';
				  print: self upperBound ].
		  self signaler ifNotNil: [
			  stream
				  nextPutAll: ' in ';
				  print: self signaler ] ]
]

{ #category : #accessing }
SubscriptOutOfBounds >> subscript [
	^ subscript
]

{ #category : #accessing }
SubscriptOutOfBounds >> subscript: anObject [
	subscript := anObject
]

{ #category : #accessing }
SubscriptOutOfBounds >> upperBound [
	^ upperBound
]

{ #category : #accessing }
SubscriptOutOfBounds >> upperBound: anObject [
	upperBound := anObject
]
