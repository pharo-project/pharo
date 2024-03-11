"
I am a behavior that can be added to debug points. 
I check if the current execution context is a test. 
The execution of behaviors continues only if the debug point is reached while running a test, otherwise it interrupts.
"
Class {
	#name : 'TestEnvironmentBehavior',
	#superclass : 'DebugPointBehavior',
	#category : 'DebugPoints-Behaviors',
	#package : 'DebugPoints',
	#tag : 'Behaviors'
}

{ #category : 'execution' }
TestEnvironmentBehavior >> execute [
	^(CurrentExecutionEnvironment value isTest).

]

{ #category : 'initialization' }
TestEnvironmentBehavior >> initialize [ 
	"should be executed before once and count etc."
	super initialize.
	self priority: 50.
]
