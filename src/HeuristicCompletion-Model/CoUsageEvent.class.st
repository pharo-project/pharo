"
I am an abstract class for events happening during code completion.
I'm used to take statistics about code completion usage.
"
Class {
	#name : 'CoUsageEvent',
	#superclass : 'Object',
	#instVars : [
		'typedToken',
		'contextClassName',
		'isDynamicContext'
	],
	#category : 'HeuristicCompletion-Model-Statistics',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Statistics'
}

{ #category : 'testing' }
CoUsageEvent class >> isAbstract [

	^ self == CoUsageEvent
]

{ #category : 'accessing' }
CoUsageEvent >> contextClassName [
	^ contextClassName
]

{ #category : 'accessing' }
CoUsageEvent >> contextClassName: anObject [
	contextClassName := anObject
]

{ #category : 'accessing' }
CoUsageEvent >> isDynamicContext [
	^ isDynamicContext
]

{ #category : 'accessing' }
CoUsageEvent >> isDynamicContext: anObject [
	isDynamicContext := anObject
]

{ #category : 'accessing' }
CoUsageEvent >> numberOfTypedCharacter [

	^ typedToken size
]

{ #category : 'accessing' }
CoUsageEvent >> typedToken [
	^ typedToken
]

{ #category : 'accessing' }
CoUsageEvent >> typedToken: anObject [
	typedToken := anObject
]
