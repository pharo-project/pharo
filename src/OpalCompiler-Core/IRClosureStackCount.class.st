"
IRClosureStackCount is used to distinguish between a stack 
in the method scope and a stack within a closure block. The
closure stack size is independent of the number of tempvars from the compiled method, therefore that number is subtracted
from this stack size length.
"
Class {
	#name : #IRClosureStackCount,
	#superclass : #IRStackCount,
	#instVars : [
		'numMethodTempVars'
	],
	#category : 'OpalCompiler-Core-Bytecode'
}

{ #category : #'instance creation' }
IRClosureStackCount class >> newOn: stack [
	^ (self startAt: stack position)
		numMethodTempVars: stack numMethodTempVars;
		yourself
]

{ #category : #initialization }
IRClosureStackCount >> initialize [

	super initialize.

	numMethodTempVars := 0.
]

{ #category : #accessing }
IRClosureStackCount >> length [

	^ super length - self numMethodTempVars
]

{ #category : #accessing }
IRClosureStackCount >> numMethodTempVars [
	^ numMethodTempVars
]

{ #category : #accessing }
IRClosureStackCount >> numMethodTempVars: nilOrNumber [
	numMethodTempVars := nilOrNumber ifNil:[0]
]
