"
I model the fact that a test generated an error.
"
Class {
	#name : 'DTErrorResultType',
	#superclass : 'DTTestResultType',
	#category : 'DrTests-TestsRunner-Results',
	#package : 'DrTests-TestsRunner',
	#tag : 'Results'
}

{ #category : 'factory' }
DTErrorResultType class >> backgroundColorStyle [

	^ 'testError'
]

{ #category : 'testing' }
DTErrorResultType >> isError [
	^ true
]

{ #category : 'accessing' }
DTErrorResultType >> name [
	^ 'Error'
]
