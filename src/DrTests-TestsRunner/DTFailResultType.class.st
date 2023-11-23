"
I model the fact that a test failed.
"
Class {
	#name : 'DTFailResultType',
	#superclass : 'DTTestResultType',
	#category : 'DrTests-TestsRunner-Results',
	#package : 'DrTests-TestsRunner',
	#tag : 'Results'
}

{ #category : 'factory' }
DTFailResultType class >> backgroundColorStyle [

	^ 'testFail'
]

{ #category : 'testing' }
DTFailResultType >> isFail [
	^ true
]

{ #category : 'accessing' }
DTFailResultType >> name [
	^ 'Failure'
]
