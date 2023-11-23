"
I model the fact that a tests expected to fail failed
"
Class {
	#name : 'DTExpectedFailureResultType',
	#superclass : 'DTTestResultType',
	#category : 'DrTests-TestsRunner-Results',
	#package : 'DrTests-TestsRunner',
	#tag : 'Results'
}

{ #category : 'factory' }
DTExpectedFailureResultType class >> backgroundColorStyle [

	^ 'testExpectedFailure'
]

{ #category : 'accessing' }
DTExpectedFailureResultType >> isExpectedFailure [
	^ true
]

{ #category : 'accessing' }
DTExpectedFailureResultType >> name [
	^ 'ExpectedFailure'
]
