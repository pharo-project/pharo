"
I am an error that is signalled in a case when a block is invoked with arguments count that does not match the count of arguments of the block.

"
Class {
	#name : #ArgumentsCountMismatch,
	#superclass : #Error,
	#instVars : [
		'expectedArgumentsCount',
		'calledArgumentsCount'
	],
	#category : #'Kernel-Exceptions'
}

{ #category : #signalling }
ArgumentsCountMismatch class >> signalExpectedArgumentsCount: expected calledArgumentsCount: called [ 

	self new 
		expectedArgumentsCount: expected;
		calledArgumentsCount: called;
		signal
]

{ #category : #accessing }
ArgumentsCountMismatch >> calledArgumentsCount [
	^ calledArgumentsCount
]

{ #category : #accessing }
ArgumentsCountMismatch >> calledArgumentsCount: anObject [
	calledArgumentsCount := anObject
]

{ #category : #accessing }
ArgumentsCountMismatch >> expectedArgumentsCount [
	^ expectedArgumentsCount
]

{ #category : #accessing }
ArgumentsCountMismatch >> expectedArgumentsCount: anObject [
	expectedArgumentsCount := anObject
]

{ #category : #accessing }
ArgumentsCountMismatch >> messageText [

	^ 'This block accepts ', (self printArgumentsCount: expectedArgumentsCount),  
			', but was called with ', (self printArgumentsCount: calledArgumentsCount), '.'
]

{ #category : #accessing }
ArgumentsCountMismatch >> printArgumentsCount: aNumber [
			
	^ aNumber printString, ' argument', (aNumber = 1 ifTrue: [''] ifFalse:['s']) 
]
