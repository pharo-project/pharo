Class {
	#name : #OCCodeReparatorTest,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-FromOld'
}

{ #category : #tests }
OCCodeReparatorTest >> testDeclareClassVar [

	| requestor method goo |
	requestor := OCMockRequestor new.
	requestor text: 'griffle ^ goo'.

	goo := MockForCompilation classVariableNamed: #goo ifAbsent: [ nil ].
	goo ifNotNil: [ MockForCompilation removeClassVariable: goo ].

	method := [ OpalCompiler new requestor: requestor; class: MockForCompilation ; compile: requestor text ]
		on: OCUndeclaredVariableWarning
		do: [ :e |
			e reparator declareClassVar.
			e retry ].

	self assert: requestor text withSeparatorsCompacted equals: 'griffle ^ goo'.
	self assert: method isCompiledMethod.
	goo := MockForCompilation classVariableNamed: #goo.
	self assert: method literals first equals: goo.

	MockForCompilation removeClassVariable: goo
]

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
OCCodeReparatorTest >> testDeclareInstVar [

	| requestor method |
	requestor := OCMockRequestor new.
	requestor text: 'griffle ^ goo'.

	(MockForCompilation hasInstVarNamed: #goo) ifTrue: [
		MockForCompilation removeInstVarNamed: #goo ].

	self deny: (MockForCompilation hasInstVarNamed: #goo).

	method := [ OpalCompiler new requestor: requestor; class: MockForCompilation ; compile: requestor text ]
		on: OCUndeclaredVariableWarning
		do: [ :e |
			e reparator declareInstVar: #goo.
			e retry ].

	self assert: requestor text withSeparatorsCompacted equals: 'griffle ^ goo'.
	self assert: method isCompiledMethod.
	self assert: (MockForCompilation hasInstVarNamed: #goo).

	MockForCompilation removeInstVarNamed: #goo
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
OCCodeReparatorTest >> testPossibleVariablesFor [

	| requestor names |
	requestor := OCMockRequestor new.
	requestor text: 'griffle | foo | ^ goo'.

	names := [ OpalCompiler new requestor: requestor; compile: requestor text ]
		on: OCUndeclaredVariableWarning
		do: [ :e |
			e reparator
				requestor: requestor;
				possibleVariablesFor: #goo.
			].

	self assert: (names includes: #foo)
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
