Class {
	#name : #NewMethodFinderSend,
	#superclass : #Object,
	#instVars : [
		'receiver',
		'selector',
		'arguments'
	],
	#category : #NewMethodFinder
}

{ #category : #acccessing }
NewMethodFinderSend >> arguments [

	^arguments
]

{ #category : #'public access' }
NewMethodFinderSend >> evaluate [
	"comment stating purpose of message"
	
	^ receiver perform: selector withArguments: arguments.
]

{ #category : #acccessing }
NewMethodFinderSend >> receiver [

	^receiver
]

{ #category : #'public access' }
NewMethodFinderSend >> resultIn: r [
	"comment stating purpose of message"
	|evaluation|
	
	[evaluation := self evaluate] on: Exception do: [ ^false ].
	^ evaluation = r.
]

{ #category : #accessing }
NewMethodFinderSend >> selector [
	
	^selector
]

{ #category : #'as yet unclassified' }
NewMethodFinderSend >> to:r selector:s1 withArgs: args [
	"comment stating purpose of message"

	receiver  := r.
	selector := s1.
	arguments := args.
]

{ #category : #'as yet unclassified' }
NewMethodFinderSend >> to:r withArgs: args [
	"comment stating purpose of message"

	receiver  := r.
	arguments := args.
]
