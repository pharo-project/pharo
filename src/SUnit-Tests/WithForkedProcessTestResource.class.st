"
I am a TestResource for testing whether my forked processes do not get terminated
between individual TestCases being executed.
"
Class {
	#name : #WithForkedProcessTestResource,
	#superclass : #TestResource,
	#instVars : [
		'forkedProcess'
	],
	#category : #'SUnit-Tests-Resources'
}

{ #category : #accessing }
WithForkedProcessTestResource >> forkedProcess [

	"Answer the receiver's forked process"

	^ forkedProcess
]

{ #category : #running }
WithForkedProcessTestResource >> setUp [

	"Create a forked process which should live until the #tearDown message is received.
	The process is and should remain in suspended state."

	super setUp.
	forkedProcess := [ "empty process" ] newProcess
]

{ #category : #running }
WithForkedProcessTestResource >> tearDown [

	"Terminate forked process"

	super tearDown.
	forkedProcess ifNotNil: [ forkedProcess terminate ]
]
