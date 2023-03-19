Class {
	#name : #OCCodeReparatorTest,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-FromOld'
}

{ #category : #tests }
OCCodeReparatorTest >> testDeclareGlobal [

	| requestor method |
	requestor := OCMockRequestor new.
	requestor text: 'griffle ^ goo'.

	Smalltalk globals removeKey: #goo ifAbsent: [].

	method := [ OpalCompiler new requestor: requestor; compile: requestor text ]
		on: OCUndeclaredVariableWarning
		do: [ :e |
			e reparator declareGlobal.
			e retry ].

	self assert: requestor text withSeparatorsCompacted equals: 'griffle ^ goo'.
	self assert: method isCompiledMethod.
	self assert: method literals first equals: (Smalltalk globals associationAt: #goo).
	self assert: method sourceCode withSeparatorsCompacted equals: 'griffle ^ goo'.

	Smalltalk globals removeKey: #goo ifAbsent: []
]

{ #category : #tests }
OCCodeReparatorTest >> testDeclareTempAndPaste [

	| requestor method |
	requestor := OCMockRequestor new.
	requestor text: 'griffle ^ goo'.

	method := [ OpalCompiler new requestor: requestor; compile: requestor text ]
		on: OCUndeclaredVariableWarning
		do: [ :e |
			e reparator
				requestor: requestor;
				declareTempAndPaste: 'goo'.
			e retry ].

	self assert: requestor text withSeparatorsCompacted equals: 'griffle | goo | ^ goo'.
	self assert: method isCompiledMethod.
	self assert: method sourceCode withSeparatorsCompacted equals: 'griffle | tmp1 | ^ tmp1' "good enough"
]

{ #category : #tests }
OCCodeReparatorTest >> testSubstituteVariableAtInterval [

	| requestor method |
	requestor := OCMockRequestor new.
	requestor text: 'griffle | foo | ^ goo'.

	method := [ OpalCompiler new requestor: requestor; compile: requestor text ]
		on: OCUndeclaredVariableWarning
		do: [ :e |
			e reparator
				requestor: requestor;
				substituteVariable: 'foo' atInterval: e node sourceInterval.
			e retry ].

	self assert: requestor text withSeparatorsCompacted equals: 'griffle | foo | ^ foo'.
	self assert: method isCompiledMethod.
	self assert: method sourceCode withSeparatorsCompacted equals: 'griffle | tmp1 | ^ tmp1' "good enough"
]

{ #category : #tests }
OCCodeReparatorTest >> testUndeclaredVariable [

	| requestor method flag |
	requestor := OCMockRequestor new.
	requestor text: 'griffle ^ goo'.

	flag := false.
	method := [ OpalCompiler new requestor: requestor; compile: requestor text ]
		on: OCUndeclaredVariableWarning
		do: [ :e |
			flag := true.
			e pass "do nothing" ].

	self assert: flag.
	self assert: method isCompiledMethod.
	self assert: method literals first isUndeclaredVariable.
	self assert: method sourceCode withSeparatorsCompacted equals: 'griffle ^ goo'
]
