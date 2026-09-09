"
I define a strategy interface to execute a test case for `NewTestRunner`. 
For simpler clases, a block can be provided to `ExecutionBlock class>>#executionBlock:`. 
If you consider it more appropiate, implement your own `ExecutionMode>>#executeCase:`

The following modes are provided through class methods:
- `ExecutionMode class >> #executionMode` runs a test case.
- `ExecutionMode class >> #debugMode` debugs a test case. 
"
Class {
	#name : 'ExecutionMode',
	#superclass : 'Object',
	#category : 'TestRunner-Core-ExecutionModes',
	#package : 'TestRunner-Core',
	#tag : 'ExecutionModes'
}

{ #category : 'instance creation' }
ExecutionMode class >> debugMode [

	^ ExecutionBlock executionBlock: [ :aTestCase | aTestCase debug ]
]

{ #category : 'testing' }
ExecutionMode class >> isAbstract [ 

	^ self == ExecutionMode 
]

{ #category : 'instance creation' }
ExecutionMode class >> runTestsMode [

	^ ExecutionBlock executionBlock: [ :aTestCase | aTestCase run ]
]

{ #category : 'execution' }
ExecutionMode >> executeCase: aTestCase [
	"executes the giving test case then returns the result"
	^ self subclassResponsibility 
]
