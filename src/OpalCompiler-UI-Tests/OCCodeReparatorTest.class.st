Class {
	#name : 'OCCodeReparatorTest',
	#superclass : 'TestCase',
	#category : 'OpalCompiler-UI-Tests',
	#package : 'OpalCompiler-UI-Tests'
}

{ #category : 'tests' }
OCCodeReparatorTest >> testDeclareClassVar [

	| requestor method goo |
	requestor := OCMockRequestor new.
	requestor text: 'griffle ^ goo'.

	goo := MockForCompilation classVariableNamed: #goo ifAbsent: [ nil ].
	goo ifNotNil: [ MockForCompilation removeClassVariable: goo ].

	method := [ OCInteractiveAPI new compile: requestor text notifying: nil in: MockForCompilation ]
		          on: OCUndeclaredVariableWarning , OCCodeError
		          do: [ :e |
				          OCCodeReparator new
					          node: e notice node;
					          declareClassVar.
				          e retry ].

	self assert: requestor text withSeparatorsCompacted equals: 'griffle ^ goo'.
	self assert: method isCompiledMethod.
	goo := MockForCompilation classVariableNamed: #goo.
	self assert: method literals first equals: goo.

	MockForCompilation removeClassVariable: goo
]

{ #category : 'tests' }
OCCodeReparatorTest >> testDeclareGlobal [

	| requestor method |
	requestor := OCMockRequestor new.
	requestor text: 'griffle ^ goo'.

	Smalltalk globals removeKey: #goo ifAbsent: [ ].

	method := [ OCInteractiveAPI new compile: requestor text notifying: nil in: MockForCompilation ]
		          on: OCUndeclaredVariableWarning , OCCodeError
		          do: [ :e |
				          OCCodeReparator new
					          node: e notice node;
					          declareGlobal.
				          e retry ].

	self assert: requestor text withSeparatorsCompacted equals: 'griffle ^ goo'.
	self assert: method isCompiledMethod.
	self assert: method literals first equals: (Smalltalk globals associationAt: #goo).
	self assert: method sourceCode withSeparatorsCompacted equals: 'griffle ^ goo'.

	Smalltalk globals removeKey: #goo ifAbsent: [ ]
]

{ #category : 'tests' }
OCCodeReparatorTest >> testDeclareInstVar [

	| requestor method |
	requestor := OCMockRequestor new.
	requestor text: 'griffle ^ goo'.

	(MockForCompilation hasInstVarNamed: #goo) ifTrue: [ MockForCompilation removeInstVarNamed: #goo ].

	self deny: (MockForCompilation hasInstVarNamed: #goo).

	method := [ OCInteractiveAPI new compile: requestor text notifying: nil in: MockForCompilation ]
		          on: OCUndeclaredVariableWarning , OCCodeError
		          do: [ :e |
				          OCCodeReparator new
					          node: e notice node;
					          declareInstVar: #goo.
				          e retry ].

	self assert: requestor text withSeparatorsCompacted equals: 'griffle ^ goo'.
	self assert: method isCompiledMethod.
	self assert: (MockForCompilation hasInstVarNamed: #goo).

	MockForCompilation removeInstVarNamed: #goo
]

{ #category : 'tests' }
OCCodeReparatorTest >> testDeclareTempAndPaste [

	| requestor method |
	requestor := OCMockRequestor new.
	requestor text: 'griffle ^ goo'.

	method := [ OCInteractiveAPI new compile: requestor text notifying: nil in: MockForCompilation ]
		          on: OCUndeclaredVariableWarning , OCCodeError
		          do: [ :e |
				          OCCodeReparator new
					          node: e notice node;
					          requestor: requestor;
					          declareTempAndPaste: 'goo'.
				          e retry ].

	self assert: requestor text withSeparatorsCompacted equals: 'griffle | goo | ^ goo'.
	self assert: method isCompiledMethod.
	self assert: method sourceCode withSeparatorsCompacted equals: 'griffle | goo | ^ goo'
]

{ #category : 'tests' }
OCCodeReparatorTest >> testPossibleVariablesFor [

	| requestor names |
	requestor := OCMockRequestor new.
	requestor text: 'griffle | foo | ^ goo'.

	names := [ OCInteractiveAPI new compile: requestor text notifying: nil in: MockForCompilation ]
		on: OCUndeclaredVariableWarning , OCCodeError
		do: [ :e |
			OCCodeReparator new
				node: e notice node;
				requestor: requestor;
				possibleVariablesFor: #goo.
			].

	self assert: (names includes: #foo)
]

{ #category : 'tests' }
OCCodeReparatorTest >> testSubstituteVariableAtInterval [

	| requestor method |
	requestor := OCMockRequestor new.
	requestor text: 'griffle | foo | ^ goo'.

	method := [ OCInteractiveAPI new compile: requestor text notifying: nil in: MockForCompilation ]
		on: OCUndeclaredVariableWarning , OCCodeError
		do: [ :e |
			OCCodeReparator new node: e notice node;
				requestor: requestor;
				substituteVariable: 'foo' atInterval: e node sourceInterval.
			e retry ].

	self assert: requestor text withSeparatorsCompacted equals: 'griffle | foo | ^ foo'.
	self assert: method isCompiledMethod.
	self assert: method sourceCode withSeparatorsCompacted equals: 'griffle | foo | ^ foo'
]

{ #category : 'tests' }
OCCodeReparatorTest >> testUndeclaredVariable [

	| requestor method |
	requestor := OCMockRequestor new.
	requestor text: 'griffle ^ goo'.

	method := OCInteractiveAPI new
		permitUndeclared: true;
		compile: requestor text notifying: nil in: MockForCompilation.

	self assert: method isCompiledMethod.
	self assert: method literals first isUndeclaredVariable.
	self assert: method sourceCode withSeparatorsCompacted equals: 'griffle ^ goo'
]
