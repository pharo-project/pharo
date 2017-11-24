"
A link to a hidden mail message.  Clicking on it allows the message to be viewed or saved to disk.
"
Class {
	#name : #TextMessageLink,
	#superclass : #TextAttribute,
	#instVars : [
		'message'
	],
	#category : #'Network-Mail'
}

{ #category : #'instance creation' }
TextMessageLink class >> message: aMessage [
	^self new initialize: aMessage
]

{ #category : #appearance }
TextMessageLink >> emphasizeScanner: scanner [
	scanner textColor: Color brown
]

{ #category : #initialization }
TextMessageLink >> initialize: message0 [
	message := message0
]

{ #category : #accessing }
TextMessageLink >> message [
	^message
]
