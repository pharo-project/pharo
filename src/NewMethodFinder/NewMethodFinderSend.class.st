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

{ #category : #printing }
NewMethodFinderSend >> printOn: aStream [

	aStream nextPutAll: receiver asString.
	aStream space.
	arguments isEmpty ifTrue: [^ aStream nextPutAll: selector].
	arguments with: selector keywords do: [:arg :word |
		aStream nextPutAll: word asString.
		aStream space.
		aStream nextPutAll: arg asString.
		aStream space.
	].
	aStream skip: -1.
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
