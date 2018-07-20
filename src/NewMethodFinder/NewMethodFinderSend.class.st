"
I am a message send that can be tested against an expected result.
If the result of the message send does not correpond to the expected result, or the message send provoked an error, or the comparison raised an error I return false.
"
Class {
	#name : #NewMethodFinderSend,
	#superclass : #Object,
	#instVars : [
		'receiver',
		'selector',
		'arguments'
	],
	#category : #'NewMethodFinder-Core'
}

{ #category : #'instance creation' }
NewMethodFinderSend class >> receiver: r selector: s1 withArguments: args [

	^ self new
		receiver: r;
		selector: s1;
		arguments: args;
		yourself
]

{ #category : #accessing }
NewMethodFinderSend >> arguments [

	^arguments
]

{ #category : #accessing }
NewMethodFinderSend >> arguments: anObject [
	arguments := anObject
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

{ #category : #accessing }
NewMethodFinderSend >> receiver [

	^receiver
]

{ #category : #accessing }
NewMethodFinderSend >> receiver: anObject [
	receiver := anObject
]

{ #category : #'public access' }
NewMethodFinderSend >> resultIn: expectedResult [
	"comment stating purpose of message"

	[ [ ^ expectedResult = (receiver perform: selector withArguments: arguments) ]
		ifError: [ :aString :aReceiver | ^ false ] ]
			on: Deprecation
			do: [ :depr | ^ false ]
]

{ #category : #accessing }
NewMethodFinderSend >> selector [
	
	^selector
]

{ #category : #accessing }
NewMethodFinderSend >> selector: anObject [
	selector := anObject
]
