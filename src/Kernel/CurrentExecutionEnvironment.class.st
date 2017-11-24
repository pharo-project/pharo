"
I am container for special meta object ExecutionEnvironment which represent current execution environment for active process.

Look at ExecutionEnvironment for details
"
Class {
	#name : #CurrentExecutionEnvironment,
	#superclass : #ProcessLocalVariable,
	#category : #'Kernel-Processes'
}

{ #category : #controlling }
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

{ #category : #testing }
CurrentExecutionEnvironment class >> isInheritable [
	^true
]

{ #category : #controlling }
CurrentExecutionEnvironment class >> restoreDefault [

	| current |
	current := self value.
	current deactivated.
	self value: nil
]

{ #category : #accessing }
CurrentExecutionEnvironment >> default [
	^DefaultExecutionEnvironment instance
]

{ #category : #inheriting }
CurrentExecutionEnvironment >> installValue: anExecutionEnvironment intoForked: newProcess from: ownerProcess [
	super installValue: anExecutionEnvironment intoForked: newProcess from: ownerProcess.
	
	anExecutionEnvironment prepareForNewProcess: newProcess
]
