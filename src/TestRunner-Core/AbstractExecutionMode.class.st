"
I am an abstract execution mode. I defined the way a testsuite should be executed (running simply all the suite, opening the debuger on the first failure or error...)
"
Class {
	#name : 'AbstractExecutionMode',
	#superclass : 'Object',
	#category : 'TestRunner-Core-ExecutionModes',
	#package : 'TestRunner-Core',
	#tag : 'ExecutionModes'
}

{ #category : 'testing' }
AbstractExecutionMode class >> debugExecutionModeClass [

	^ DebugExecutionMode
]

{ #category : 'execution' }
AbstractExecutionMode class >> executeCase: aTestCase [
	"executes the giving test case then returns the result"

	self subclassResponsibility
]

{ #category : 'testing' }
AbstractExecutionMode class >> isAbstract [

	^ self == AbstractExecutionMode
]

{ #category : 'testing' }
AbstractExecutionMode class >> runExecutionModeClass [

	^ RunExecutionMode
]
