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

{ #category : #tests }
OCCompilerExceptionsTest >> testUndeclaredVariableRepairWithGlobal [

	| requestor method |
	requestor := OCMockRequestor new.
	requestor text: 'griffle ^ goo'.

	Smalltalk globals removeKey: #goo ifAbsent: [].

	method := [ OpalCompiler new requestor: requestor; compile: requestor text ]
		on: OCUndeclaredVariableWarning
		do: [ :e |
			e declareGlobal.
			e retry ].

	self assert: requestor text withSeparatorsCompacted equals: 'griffle ^ goo'.
	self assert: method isCompiledMethod.
	self assert: method literals first equals: (Smalltalk globals associationAt: #goo).
	self assert: method sourceCode withSeparatorsCompacted equals: 'griffle ^ goo'.

	Smalltalk globals removeKey: #goo ifAbsent: []
]

{ #category : #tests }
OCCompilerExceptionsTest >> testUndeclaredVariableRepairWithTemp [

	| requestor method |
	requestor := OCMockRequestor new.
	requestor text: 'griffle ^ goo'.

	method := [ OpalCompiler new requestor: requestor; compile: requestor text ]
		on: OCUndeclaredVariableWarning
		do: [ :e |
			e declareTempAndPaste: 'goo'.
			e retry ].

	self assert: requestor text withSeparatorsCompacted equals: 'griffle | goo | ^ goo'.
	self assert: method isCompiledMethod.
	self assert: method sourceCode withSeparatorsCompacted equals: 'griffle | tmp1 | ^ tmp1' "good enough"
]

{ #category : #tests }
OCCompilerExceptionsTest >> testUndeclaredVariableRepairWithTemp2 [

	| requestor method |
	requestor := OCMockRequestor new.
	requestor text: 'griffle | foo | ^ goo'.

	method := [ OpalCompiler new requestor: requestor; compile: requestor text ]
		on: OCUndeclaredVariableWarning
		do: [ :e |
			e substituteVariable: 'foo' atInterval: e node sourceInterval.
			e retry ].

	self assert: requestor text withSeparatorsCompacted equals: 'griffle | foo | ^ foo'.
	self assert: method isCompiledMethod.
	self assert: method sourceCode withSeparatorsCompacted equals: 'griffle | tmp1 | ^ tmp1' "good enough"
]
