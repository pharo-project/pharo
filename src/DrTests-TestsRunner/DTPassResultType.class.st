"
I model the fact that a test passed.
"
Class {
	#name : 'DTPassResultType',
	#superclass : 'DTTestResultType',
	#category : 'DrTests-TestsRunner-Results',
	#package : 'DrTests-TestsRunner',
	#tag : 'Results'
}

{ #category : 'factory' }
DTPassResultType class >> backgroundColorStyle [

	^ 'testPass'
]

{ #category : 'testing' }
DTPassResultType >> isPass [
	^ true
]

{ #category : 'accessing' }
DTPassResultType >> name [
	^ 'Passing test'
]
