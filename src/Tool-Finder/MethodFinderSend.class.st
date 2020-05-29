"
I am a message send that can be tested against an expected result.
If the result of the message send does not correpond to the expected result, or the message send provoked an error, or the comparison raised an error I return false.
"
Class {
	#name : #MethodFinderSend,
	#superclass : #Object,
	#instVars : [
		'receiver',
		'selector',
		'arguments'
	],
	#category : #'Tool-Finder-MethodFinder\r\t'
}

{ #category : #'instance creation' }
MethodFinderSend class >> receiver: r selector: s1 withArguments: args [

	^ self new
		receiver: r;
		selector: s1;
		arguments: args;
		yourself
]

{ #category : #accessing }
MethodFinderSend >> arguments [

	^arguments
]

{ #category : #accessing }
MethodFinderSend >> arguments: anObject [
	arguments := anObject
]

{ #category : #'public access' }
MethodFinderSend >> evaluate [
	"comment stating purpose of message"
	
	^ receiver perform: selector withArguments: arguments.
]

{ #category : #printing }
MethodFinderSend >> printOn: aStream [

	aStream print: receiver.
	aStream space.
	arguments ifEmpty: [^ aStream nextPutAll: selector].
	arguments with: selector keywords do: [:arg :word |

		aStream nextPutAll: word asString.
		aStream space.
		aStream print: arg.
		aStream space.

	].
	aStream skip: -1.
]

{ #category : #accessing }
MethodFinderSend >> receiver [

	^receiver
]

{ #category : #accessing }
MethodFinderSend >> receiver: anObject [
	receiver := anObject
]

{ #category : #'public access' }
MethodFinderSend >> resultIn: expectedResult [
	"comment stating purpose of message"

	[ [ ^ expectedResult = (receiver perform: selector withArguments: arguments) ]
		onErrorDo: [ :anError | ^ false ] ]
			on: Deprecation
			do: [ :depr | ^ false ]
]

{ #category : #accessing }
MethodFinderSend >> selector [
	
	^selector
]

{ #category : #accessing }
MethodFinderSend >> selector: anObject [
	selector := anObject
]
