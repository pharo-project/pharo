Class {
	#name : #CoBenchmarkContext,
	#superclass : #Object,
	#instVars : [
		'callsite',
		'position'
	],
	#category : #'HeuristicCompletion-Model-Benchmarks'
}

{ #category : #accessing }
CoBenchmarkContext >> callsite [
	^ callsite
]

{ #category : #accessing }
CoBenchmarkContext >> callsite: anObject [
	callsite := anObject
]

{ #category : #accessing }
CoBenchmarkContext >> completionClass [
	
	^ callsite methodNode methodClass
]

{ #category : #accessing }
CoBenchmarkContext >> doItContext [
	
	^ nil
]

{ #category : #accessing }
CoBenchmarkContext >> doItRequestor [

	^ nil
]

{ #category : #accessing }
CoBenchmarkContext >> environmentAt: aString ifPresent: aBlockClosure [ 
	
	self class environment at: aString ifPresent: aBlockClosure
]

{ #category : #testing }
CoBenchmarkContext >> isWorkspace [
	
	^ false
]

{ #category : #accessing }
CoBenchmarkContext >> position [
	
	^ position first + 1
]

{ #category : #accessing }
CoBenchmarkContext >> position: aPosition [
	
	position := aPosition
]

{ #category : #accessing }
CoBenchmarkContext >> source [
	
	^ callsite methodNode method sourceCode
]
