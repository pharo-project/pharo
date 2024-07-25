"
I am container for special meta object ExecutionEnvironment which represent current execution environment for active process.

Look at ExecutionEnvironment for details
"
Class {
	#name : 'CurrentExecutionEnvironment',
	#superclass : 'ProcessLocalVariable',
	#category : 'SUnit-Core-Execution-Environments',
	#package : 'SUnit-Core',
	#tag : 'Execution-Environments'
}

{ #category : 'controlling' }
CurrentExecutionEnvironment class >> activate: anExecutionEnvironment for: aBlock [

	| current |
	self value == anExecutionEnvironment ifTrue: [ ^aBlock value ].

	current := self soleInstance valueOrNil.
	[ self value: anExecutionEnvironment.
	anExecutionEnvironment activated.
	aBlock value  ] ensure: [
		self value: current.
		anExecutionEnvironment deactivated]
]

{ #category : 'testing' }
CurrentExecutionEnvironment class >> isInheritable [
	^true
]

{ #category : 'controlling' }
CurrentExecutionEnvironment class >> restoreDefault [

	| current |
	current := self value.
	current deactivated.
	self value: nil
]

{ #category : 'controlling' }
CurrentExecutionEnvironment class >> runTestCase: aTestCase [

	self value runTestCase: aTestCase
]

{ #category : 'controlling' }
CurrentExecutionEnvironment class >> runTestsBy: aBlock [

	self value runTestsBy: aBlock
]

{ #category : 'accessing' }
CurrentExecutionEnvironment >> default [
	^DefaultExecutionEnvironment instance
]

{ #category : 'inheriting' }
CurrentExecutionEnvironment >> installValue: anExecutionEnvironment intoForked: newProcess from: ownerProcess [
	super installValue: anExecutionEnvironment intoForked: newProcess from: ownerProcess.

	anExecutionEnvironment prepareForNewProcess: newProcess
]
