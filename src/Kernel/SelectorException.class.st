"
I am SelectorException, an abstract superclass for exceptions related to a selector.
"
Class {
	#name : #SelectorException,
	#superclass : #Error,
	#instVars : [
		'selector'
	],
	#category : 'Kernel-Exceptions'
}

{ #category : #signaling }
SelectorException class >> signalFor: aSelector [
	"Create and signal an exception for aSelector in the default receiver."
	
	^ self new
		selector: aSelector;
		signal
]

{ #category : #signaling }
SelectorException class >> signalFor: aSelector in: aReceiver [
	"Create and signal an exception for aSelector in aReceiver."

	^ self new
		selector: aSelector;
		signaler: aReceiver;
		signal
]

{ #category : #accessing }
SelectorException >> messageText [
	"Overwritten to initialiaze the message text to a standard text if it has not yet been set"
	
	^ messageText ifNil: [ messageText := self standardMessageText ]
]

{ #category : #accessing }
SelectorException >> selector [
	^ selector
]

{ #category : #accessing }
SelectorException >> selector: aSelector [
	selector := aSelector
]

{ #category : #printing }
SelectorException >> standardMessageText [
	"Generate a standard textual description"
	
	^ String streamContents: [ :stream |
		stream << 'for '.
		stream print: self selector.
		stream << ' in '.
		stream print: self signaler class]
]
