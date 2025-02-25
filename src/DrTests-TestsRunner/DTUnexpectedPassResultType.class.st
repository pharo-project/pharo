"
I model the fact that a test expected to fail passed
"
Class {
	#name : 'DTUnexpectedPassResultType',
	#superclass : 'DTTestResultType',
	#category : 'DrTests-TestsRunner-Results',
	#package : 'DrTests-TestsRunner',
	#tag : 'Results'
}

{ #category : 'factory' }
DTUnexpectedPassResultType class >> backgroundColorStyle [

	^ 'testUnexpectedPass'
]

{ #category : 'accessing' }
DTUnexpectedPassResultType >> isUnexpectedPass [

	^ true
]

{ #category : 'accessing' }
DTUnexpectedPassResultType >> name [
	^ 'Unexpected passed test'
]
