Class {
	#name : #Send,
	#superclass : #Object,
	#instVars : [
		'receiver',
		'selector',
		'arguments'
	],
	#category : #NewMethodFinder
}

{ #category : #acccessing }
Send >> arguments [

	^arguments
]

{ #category : #'public access' }
Send >> evaluate [
	"comment stating purpose of message"
	
	^ receiver perform: selector withArguments: arguments.
]

{ #category : #acccessing }
Send >> receiver [

	^receiver
]

{ #category : #'public access' }
Send >> resultIn: r [
	"comment stating purpose of message"
	|evaluation|
	
	[evaluation := self evaluate] on: Exception do: [ ^false ].
	^ evaluation = r.
]

{ #category : #accessing }
Send >> selector [
	
	^selector
]

{ #category : #'as yet unclassified' }
Send >> to:r selector:s1 withArgs: args [
	"comment stating purpose of message"

	receiver  := r.
	selector := s1.
	arguments := args.
]

{ #category : #'as yet unclassified' }
Send >> to:r withArgs: args [
	"comment stating purpose of message"

	receiver  := r.
	arguments := args.
]
