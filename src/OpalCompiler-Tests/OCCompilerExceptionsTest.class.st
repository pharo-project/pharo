Class {
	#name : #OCCompilerExceptionsTest,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-FromOld'
}

{ #category : #tests }
OCCompilerExceptionsTest >> testUndeclaredVariable [

	| requestor |
	requestor := OCMockRequestor new.
	requestor text: 'griffle ^ goo'.
	self
		should: [
			OpalCompiler new
				requestor: requestor;
				compile: requestor text ]
		raise: OCUndeclaredVariableWarning
]
