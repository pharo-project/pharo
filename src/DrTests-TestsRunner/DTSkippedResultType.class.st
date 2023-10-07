"
I model the fact that a test was skipped.
"
Class {
	#name : 'DTSkippedResultType',
	#superclass : 'DTTestResultType',
	#category : 'DrTests-TestsRunner-Results',
	#package : 'DrTests-TestsRunner',
	#tag : 'Results'
}

{ #category : 'factory' }
DTSkippedResultType class >> backgroundColorStyle [

	^ 'testSkipped'
]

{ #category : 'testing' }
DTSkippedResultType >> isSkipped [
	^ true
]

{ #category : 'accessing' }
DTSkippedResultType >> name [
	^ 'Skipped test'
]
