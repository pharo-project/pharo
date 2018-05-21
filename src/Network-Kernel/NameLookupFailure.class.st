"
Signals that a name lookup operation failed.

	hostName	hostName for which the name loopup failed

"
Class {
	#name : #NameLookupFailure,
	#superclass : #NetworkError,
	#instVars : [
		'hostName'
	],
	#category : #'Network-Kernel-Exceptions'
}

{ #category : #'instance creation' }
NameLookupFailure class >> hostName: aString [
	^ self new hostName: aString
]

{ #category : #'instance creation' }
NameLookupFailure class >> signalFor: hostName [
	"Create and signal a NameLookupFailure for hostName"

	^ (self hostName: hostName) signal
]

{ #category : #accessing }
NameLookupFailure >> defaultAction [
	"Backward compatibility"

	| response |
	response := UIManager default chooseFrom: #('Retry' 'Give Up') title: self printString.
	^ response = 2
		ifTrue: [ super defaultAction ]
		ifFalse: [ self retry ]
]

{ #category : #accessing }
NameLookupFailure >> hostName [
	^ hostName
]

{ #category : #accessing }
NameLookupFailure >> hostName: aString [
	hostName := aString
]

{ #category : #accessing }
NameLookupFailure >> messageText [
	"Overwritten to initialiaze the message text to a standard text if it has not yet been set"

	^ messageText ifNil: [ messageText := self standardMessageText ]
]

{ #category : #accessing }
NameLookupFailure >> standardMessageText [
	"Generate a standard textual description"

	^ String
		streamContents: [ :stream | 
			stream << 'cannot resolve '.
			stream print: self hostName ]
]
