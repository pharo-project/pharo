"
An ExecutionEnvironmentTest is a test class for testing the behavior of ExecutionEnvironment
"
Class {
	#name : 'ExecutionEnvironmentTest',
	#superclass : 'TestCase',
	#category : 'SUnit-Tests',
	#package : 'SUnit-Tests'
}

{ #category : 'tests' }
ExecutionEnvironmentTest >> testExecutionEnvironment [

	| newEnv |
	newEnv := ExecutionEnvironmentStub new.
	newEnv beActiveDuring: [
		self assert: CurrentExecutionEnvironment value equals: newEnv.

		DefaultExecutionEnvironment beActiveDuring: [
			self assert: CurrentExecutionEnvironment value equals: DefaultExecutionEnvironment instance].

		self assert: CurrentExecutionEnvironment value equals: newEnv.
	]
]

{ #category : 'tests' }
ExecutionEnvironmentTest >> testInheritingExecutionEnvironment [

	| newEnv p |
	newEnv := ExecutionEnvironmentStub new.
	newEnv beActiveDuring: [ p := [ self assert: CurrentExecutionEnvironment value equals: newEnv ] fork ].

	self assert: newEnv forkedProcesses anyOne equals: p
]
