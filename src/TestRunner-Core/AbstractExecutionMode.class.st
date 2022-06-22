"
I am an abstract execution mode. I defined the way a testsuite should be executed (running simply all the suite, opening the debuger on the first failure or error...)
"
Class {
	#name : #AbstractExecutionMode,
	#superclass : #Object,
	#category : #'TestRunner-Core-Configuration'
}

{ #category : #execution }
AbstractExecutionMode class >> defaultTestResultClass [

	^ TestResult
]

{ #category : #execution }
AbstractExecutionMode class >> executeSuite: aTestSuite [
	"executes the giving test suite then returns the result"

	self subclassResponsibility
]

{ #category : #execution }
AbstractExecutionMode class >> isAbstract [

	^ self == AbstractExecutionMode
]
