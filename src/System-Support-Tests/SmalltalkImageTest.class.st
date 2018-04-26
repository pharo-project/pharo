"
Tests for SmalltalkImage
"
Class {
	#name : #SmalltalkImageTest,
	#superclass : #TestCase,
	#category : #'System-Support-Tests'
}

{ #category : #'tests-arguments' }
SmalltalkImageTest >> testExtractAllKinds [
	| args extract keys |
	args := #('ArgWithoutMinus1' '-ArgWithMinus1' 'ArgWithoutMinus2' 'ArgWithoutMinus3' '-ArgWithMinus2').
	extract := SmalltalkImage current extractMinusParametersFrom: args.
	keys := extract keys sort.
	self assert: extract isDictionary.
	self assert: extract size = 3.
	self assert: (keys at: 1) = #*.
	self assert: (keys at: 2) = #ArgWithMinus1.
	self assert: (keys at: 3) = #ArgWithMinus2.
	self assert: (extract at: #*) = (Array with: 'ArgWithoutMinus1').
	self assert: (extract at: #ArgWithMinus1) = #('ArgWithoutMinus2' 'ArgWithoutMinus3').
	self assert: (extract at: #ArgWithMinus2) isEmpty
]

{ #category : #'tests-arguments' }
SmalltalkImageTest >> testExtractEmpty [
	| args extract |
	args := #().
	extract := SmalltalkImage current extractMinusParametersFrom: args.
	self assert: extract class equals: Dictionary.
	self assert: extract isEmpty
]

{ #category : #'tests-arguments' }
SmalltalkImageTest >> testExtractNil [
	| args extract |
	args := nil.
	extract := SmalltalkImage current extractMinusParametersFrom: args.
	self assert: extract class equals: Dictionary.
	self assert: extract isEmpty
]

{ #category : #'tests-arguments' }
SmalltalkImageTest >> testExtractOneArgNoMinus [
	| args extract |
	args := #('ArgNoMinus').
	extract := SmalltalkImage current extractMinusParametersFrom: args.
	self assert: extract isDictionary.
	self assert: extract size equals: 1.
	self assert: (extract keys at: 1) equals: #*.
	self assert: (extract at: #*) equals: args
]

{ #category : #'tests-arguments' }
SmalltalkImageTest >> testExtractOneArgWithMinus [
	| args extract |
	args := #('-ArgWithMinus').
	extract := SmalltalkImage current extractMinusParametersFrom: args.
	self assert: extract isDictionary.
	self assert: extract size equals: 1.
	self assert: (extract keys at: 1) equals: #ArgWithMinus.
	self assert: (extract at: #ArgWithMinus) isEmpty
]

{ #category : #'tests-arguments' }
SmalltalkImageTest >> testExtractOneArgWithMinusThenArgsWithout [
	| args extract |
	args := #('-ArgWithMinus' 'ArgWithoutMinus1' 'ArgWithoutMinus2').
	extract := SmalltalkImage current extractMinusParametersFrom: args.
	self assert: extract isDictionary.
	self assert: extract size equals: 1.
	self assert: (extract keys at: 1) equals: #ArgWithMinus.
	self assert: (extract at: #ArgWithMinus) equals: #('ArgWithoutMinus1' 'ArgWithoutMinus2')
]

{ #category : #'tests-arguments' }
SmalltalkImageTest >> testExtractTwoArgsWithAndWithoutMinus [
	| args extract keys |
	args := #('ArgWithoutMinus' '-ArgWithMinus').
	extract := SmalltalkImage current extractMinusParametersFrom: args.
	keys := extract keys sort.
	self assert: extract isDictionary.
	self assert: extract size equals: 2.
	self assert: (keys at: 1) equals: #*.
	self assert: (keys at: 2) equals: #ArgWithMinus.
	self assert: (extract at: #*) equals: (Array with: 'ArgWithoutMinus').
	self assert: (extract at: #ArgWithMinus) isEmpty
]

{ #category : #testing }
SmalltalkImageTest >> testOpenLog [
	"It must be possible to write to the log even when the log file can''t be created."

	"Standard case where a file can be created with this name."
	self writeToLogWithFileNamed: 'file.log'.

	"Other cases where a file with the given name can not be created."
	self writeToLogWithFileNamed: ''.
	self writeToLogWithFileNamed: '.'.

]

{ #category : #helpers }
SmalltalkImageTest >> writeToLogWithFileNamed: filename [
	| oldLogFileName |
	oldLogFileName := Smalltalk logFileName.
	Smalltalk logFileName: filename.
	[ 
	| stream |
	stream := Smalltalk image openLog.
	stream nextPutAll: 'foobar'	"We just make sure we can write something. I don't know any reliable way to check if it has really been written" ]
		ensure: [ Smalltalk logFileName: oldLogFileName ]
]
