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

	method := [ OpalCompiler new class: MockForCompilation ; compile: requestor text ]
		on: OCUndeclaredVariableWarning , OCCodeError
		do: [ :e |
			e notice reparator declareClassVar.
			e retry ].

	self assert: requestor text withSeparatorsCompacted equals: 'griffle ^ goo'.
	self assert: method isCompiledMethod.
	goo := MockForCompilation classVariableNamed: #goo.
	self assert: method literals first equals: goo.

	MockForCompilation removeClassVariable: goo
]

{ #category : 'tests' }
OCCodeReparatorTest >> testDeclareClassVarBlock [

	| compiler requestor method goo |
	requestor := OCMockRequestor new.
	requestor text: 'griffle ^ goo'.

	goo := MockForCompilation classVariableNamed: #goo ifAbsent: [ nil ].
	goo ifNotNil: [ MockForCompilation removeClassVariable: goo ].

	method := (compiler := OpalCompiler new)
		class: MockForCompilation;
		failBlock: [ :notice |
			notice reparator declareClassVar.
			compiler compile ];
		compile: requestor text.

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

	Smalltalk globals removeKey: #goo ifAbsent: [].

	method := [ OpalCompiler new compile: requestor text ]
		on: OCUndeclaredVariableWarning , OCCodeError
		do: [ :e |
			e notice reparator declareGlobal.
			e retry ].

	self assert: requestor text withSeparatorsCompacted equals: 'griffle ^ goo'.
	self assert: method isCompiledMethod.
	self assert: method literals first equals: (Smalltalk globals associationAt: #goo).
	self assert: method sourceCode withSeparatorsCompacted equals: 'griffle ^ goo'.

	Smalltalk globals removeKey: #goo ifAbsent: []
]

{ #category : 'tests' }
OCCodeReparatorTest >> testDeclareGlobalBlock [

	| compiler requestor method |
	requestor := OCMockRequestor new.
	requestor text: 'griffle ^ goo'.

	Smalltalk globals removeKey: #goo ifAbsent: [].

	method := (compiler := OpalCompiler new)
		class: MockForCompilation;
		failBlock: [ :notice |
			notice reparator declareGlobal.
			compiler compile ];
		compile: requestor text.

	self assert: requestor text withSeparatorsCompacted equals: 'griffle ^ goo'.
	self assert: method isCompiledMethod.
	self assert: method literals first equals: (Smalltalk globals associationAt: #goo).
	self assert: method sourceCode withSeparatorsCompacted equals: 'griffle ^ goo'.

	Smalltalk globals removeKey: #goo ifAbsent: []
]

{ #category : 'tests' }
OCCodeReparatorTest >> testDeclareInstVar [

	| requestor method |
	requestor := OCMockRequestor new.
	requestor text: 'griffle ^ goo'.

	(MockForCompilation hasInstVarNamed: #goo) ifTrue: [
		MockForCompilation removeInstVarNamed: #goo ].

	self deny: (MockForCompilation hasInstVarNamed: #goo).

	method := [ OpalCompiler new class: MockForCompilation ; compile: requestor text ]
		on: OCUndeclaredVariableWarning , OCCodeError
		do: [ :e |
			e notice reparator declareInstVar: #goo.
			e retry ].

	self assert: requestor text withSeparatorsCompacted equals: 'griffle ^ goo'.
	self assert: method isCompiledMethod.
	self assert: (MockForCompilation hasInstVarNamed: #goo).

	MockForCompilation removeInstVarNamed: #goo
]

{ #category : 'tests' }
OCCodeReparatorTest >> testDeclareInstVarBlock [

	| compiler requestor method |
	requestor := OCMockRequestor new.
	requestor text: 'griffle ^ goo'.

	(MockForCompilation hasInstVarNamed: #goo) ifTrue: [
		MockForCompilation removeInstVarNamed: #goo ].

	self deny: (MockForCompilation hasInstVarNamed: #goo).

	method := (compiler := OpalCompiler new)
		class: MockForCompilation;
		failBlock: [ :notice |
			notice reparator declareInstVar: #goo.
			compiler compile ];
		compile: requestor text.

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

	method := [ OpalCompiler new compile: requestor text ]
		on: OCUndeclaredVariableWarning , OCCodeError
		do: [ :e |
			e notice reparator
				requestor: requestor;
				declareTempAndPaste: 'goo'.
			e retry ].

	self assert: requestor text withSeparatorsCompacted equals: 'griffle | goo | ^ goo'.
	self assert: method isCompiledMethod.
	self assert: method sourceCode withSeparatorsCompacted equals: 'griffle | goo | ^ goo'
]

{ #category : 'tests' }
OCCodeReparatorTest >> testDeclareTempAndPasteBlock [

	| compiler requestor method |
	requestor := OCMockRequestor new.
	requestor text: 'griffle ^ goo'.

	method := (compiler := OpalCompiler new)
		class: MockForCompilation;
		failBlock: [ :notice |
			notice reparator
				requestor: requestor;
				declareTempAndPaste: 'goo'.
			compiler compile: requestor text ];
		compile: requestor text.

	self assert: requestor text withSeparatorsCompacted equals: 'griffle | goo | ^ goo'.
	self assert: method isCompiledMethod.
	self assert: method sourceCode withSeparatorsCompacted equals: 'griffle | goo | ^ goo'
]

{ #category : 'tests' }
OCCodeReparatorTest >> testPossibleVariablesFor [

	| requestor names |
	requestor := OCMockRequestor new.
	requestor text: 'griffle | foo | ^ goo'.

	names := [ OpalCompiler new compile: requestor text ]
		on: OCUndeclaredVariableWarning , OCCodeError
		do: [ :e |
			e notice reparator
				requestor: requestor;
				possibleVariablesFor: #goo.
			].

	self assert: (names includes: #foo)
]

{ #category : 'tests' }
OCCodeReparatorTest >> testPossibleVariablesForBlock [

	| compiler requestor names |
	requestor := OCMockRequestor new.
	requestor text: 'griffle | foo | ^ goo'.

	names := (compiler := OpalCompiler new)
		class: MockForCompilation;
		failBlock: [ :notice |
			notice reparator possibleVariablesFor: #goo ];
		compile: requestor text.

	self assert: (names includes: #foo)
]

{ #category : 'tests' }
OCCodeReparatorTest >> testSubstituteVariableAtInterval [

	| requestor method |
	requestor := OCMockRequestor new.
	requestor text: 'griffle | foo | ^ goo'.

	method := [ OpalCompiler new compile: requestor text ]
		on: OCUndeclaredVariableWarning , OCCodeError
		do: [ :e |
			e notice reparator
				requestor: requestor;
				substituteVariable: 'foo' atInterval: e node sourceInterval.
			e retry ].

	self assert: requestor text withSeparatorsCompacted equals: 'griffle | foo | ^ foo'.
	self assert: method isCompiledMethod.
	self assert: method sourceCode withSeparatorsCompacted equals: 'griffle | foo | ^ foo'
]

{ #category : 'tests' }
OCCodeReparatorTest >> testSubstituteVariableAtIntervalBlock [

	| compiler requestor method |
	requestor := OCMockRequestor new.
	requestor text: 'griffle | foo | ^ goo'.

	method := (compiler := OpalCompiler new)
		class: MockForCompilation;
		failBlock: [ :notice |
			notice reparator
				requestor: requestor;
				substituteVariable: 'foo' atInterval: notice node sourceInterval.
			compiler compile: requestor text ];
		compile: requestor text.

	self assert: requestor text withSeparatorsCompacted equals: 'griffle | foo | ^ foo'.
	self assert: method isCompiledMethod.
	self assert: method sourceCode withSeparatorsCompacted equals: 'griffle | foo | ^ foo'
]

{ #category : 'tests' }
OCCodeReparatorTest >> testUndeclaredVariable [

	| requestor method flag |
	requestor := OCMockRequestor new.
	requestor text: 'griffle ^ goo'.

	flag := false.
	method := [ OpalCompiler new compile: requestor text ]
		on: OCUndeclaredVariableWarning , OCCodeError
		do: [ :e |
			flag := true.
			e resume "continue" ].

	self assert: flag.
	self assert: method isCompiledMethod.
	self assert: method literals first isUndeclaredVariable.
	self assert: method sourceCode withSeparatorsCompacted equals: 'griffle ^ goo'
]

{ #category : 'tests' }
OCCodeReparatorTest >> testdefineClass [
	self skip.
	"we need to resume: not with true, but the code, ProvideAnswerNotification needs to be improved
	to provide the defaultAnswer, too"
	[ OCCodeReparator new
		node: (self class>>#testdefineClass) ast;
		defineClass: 'MyTestClassForDefineClass'
	]
		on: ProvideAnswerNotification
		do: [ :e | e resume: true ].
		
	self assert: (Smalltalk globals hasClassNamed: #MyTestClassForDefineClass).
	Smalltalk globals removeClassNamed:  #MyTestClassForDefineClass
]

{ #category : 'tests' }
OCCodeReparatorTest >> testdefineTrait [
	self skip.
	"we need to resume: not with true, but the code, ProvideAnswerNotification needs to be improved
	to provide the defaultAnswer, too"
	[ OCCodeReparator new
		node: (self class>>#testdefineTrait) ast;
		defineTrait: 'MyTestTraitForDefineTrait'
	]
		on: ProvideAnswerNotification
		do: [ :e | e resume: true ].
		
	self assert: (Smalltalk traitNames includes: #MyTestTraitForDefineTrait).
	Smalltalk globals removeClassNamed:  #MyTestTraitForDefineTrait
]
