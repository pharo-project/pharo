Class {
	#name : 'RBAbstractEnvTestCase',
	#superclass : 'ParametrizedTestCase',
	#instVars : [
		'testingEnvironment',
		'model'
	],
	#category : 'Refactoring-Transformations-Tests-Parametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'utilities' }
RBAbstractEnvTestCase class >> makeAllFailureShownInTestMethodNames [
	
	| methods nSel |
	methods := OrderedCollection new.
	RBAbstractEnvTestCase withAllSubclassesDo: [ :cls |

	methods addAll: (((cls methodsInProtocol: 'failure tests') 
		select: [ :each | 'test*' match: each selector  ] )
		reject: [ :each | 'testFailure*' match: each selector ]). ].


	methods do: [ :meth |
		nSel := 'testFailure', (meth selector allButFirst: 4) asSymbol.
	meth methodClass  
	compile: (meth ast selector: nSel) formattedCode 
	classified: 'failure tests']
]

{ #category : 'utilities' }
RBAbstractEnvTestCase class >> removeDuplicates [
	
	| methods  |
	methods := OrderedCollection new.
	RBAbstractEnvTestCase withAllSubclassesDo: [ :cls |

	methods addAll: (((cls methodsInProtocol: 'failure tests') 
		select: [ :each | 'test*' match: each selector  ] )
		reject: [ :each | 'testFailure*' match: each selector ]). ].
	methods do: [ :each | 
			each methodClass removeSelector:  each selector
			 ]

	
]

{ #category : 'mocking helpers' }
RBAbstractEnvTestCase >> addMethodsToModelClasses: aCollection in: newModel [
	
	aCollection do: [:each |
			| class |
			class := newModel classNamed: each first.
			each last do: [ :methodPair |
				class
					compile: methodPair first
					classified: methodPair last]].
	^ newModel
]

{ #category : 'accessing' }
RBAbstractEnvTestCase >> defaultNamespaceClass [
	^ RBNamespace
]

{ #category : 'mocking' }
RBAbstractEnvTestCase >> defineFooBarIn: aRBNamespace [
	aRBNamespace
		defineClass: [ :aBuilder |
			aBuilder
				superclass: Object;
				name: #Foo;
				slots: { #instVarName1. #instVarName2 };
				sharedVariables: { #ClassVarName1. #ClassVarName2 };
				package: 'Testing' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #Foo;
				name: #Bar;
				package: 'Testing' ].

	self
		addMethodsToModelClasses: #( #( #Bar #( #( 'instVarName1
				^1242321' #tests ) #( 'foo
				instVarName1 := instVarName1 + instVarName2 + ClassVarName1' #tests ) ) ) #( #Foo #( #( 'foo
				^instVarName2 := 3' #tests ) #( 'bar
				"Add one to instVarName1"
				instVarName1 := instVarName1 + 1' #tests ) #( 'classVarName1
				^ClassVarName1' #tests ) #( 'instVarName1: anObject
				^anObject' #tests ) #( 'asdf
				^self classVarName1: (ClassVarName1 := ClassVarName1 + 1)' #tests ) #( 'instVarName2
				^instVarName2' #tests ) #( 'instVarName2: anObject
				instVarName2 := anObject' #tests ) #( 'classVarName1: anObject
				^ClassVarName1 := anObject' #tests ) ) ) #( 'Bar class' #( #( 'classVarName2: anObject
				ClassVarName2 := anObject' #tests ) #( 'classVarName2
				^ClassVarName2' #tests ) ) ) #( 'Foo class' #( #( 'foo
				^ClassVarName1 := ClassVarName1 * ClassVarName1 * ClassVarName1' #tests ) ) ) )
		in: aRBNamespace.
		^ aRBNamespace
]

{ #category : 'accessing' }
RBAbstractEnvTestCase >> environmentOfTest [
	^ testingEnvironment
]

{ #category : 'utilities' }
RBAbstractEnvTestCase >> modelOnClasses: aCollection [

	^ self defaultNamespaceClass onEnvironment:
		  (RBClassEnvironment classes: aCollection)
]

{ #category : 'mocking helpers' }
RBAbstractEnvTestCase >> modelWithoutRealClasses: aColOfClasses [

	| newModel classEnvironment classes |
	classes := aColOfClasses
		inject: OrderedCollection new
		into: [ :sum :each |
			self environmentOfTest at: each ifPresent: [ :class |
				sum add: class; add: class class ].
			sum ].
	classEnvironment := RBClassEnvironment classes: classes.
	newModel := self defaultNamespaceClass onEnvironment: classEnvironment not.
	newModel name: 'Test'.
	^ newModel

	
]

{ #category : 'actions' }
RBAbstractEnvTestCase >> proceedThroughWarning: aBlock [
	aBlock on: RBRefactoringWarning do: [ :ex | ex resume ]
]

{ #category : 'mocking' }
RBAbstractEnvTestCase >> rbModelForExtractMethodTest [

	| newModel |
	newModel := self modelWithoutRealClasses: #( #MyClassA #MyClassB #MyClassC ).
	newModel
		defineClass: [ :aBuilder |
			aBuilder
				superclass: Object;
				name: #MyClassA;
				slots: { #instVarName1. #instVarName2 };
				sharedVariables: { #ClassVarName1. #ClassVarName2 };
				package: 'Testing' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #MyClassA;
				name: #MyClassB;
				package: 'Testing' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #MyClassB;
				name: #MyClassC;
				package: 'Testing' ].

	self
		addMethodsToModelClasses: #( #( #MyClassA #( #( 'bar
				^ self' #tests ) #( 'name
				^ ''SomeString''' #tests ) #( 'methodWithArg: anArg
				^ anArg not' #tests ) #( 'methodWithArg: anArg1 andArg: anArg2
				^ (anArg1 > anArg2) not' #tests ) #( 'problemCount
				^ 435' #tests ) #( 'string: aString stream: aStream
				nil' ) #( 'myMethod
				| token |
				token := (String new: 100) writeStream.
				[ currentChar isLetter ] whileTrue: [
					token nextPut: currentChar.
					self methodWithArg: currentChar isLetter.
					self bar ].
				currentChar isLetter.
				$3 isLetter.
	 			currentChar isLetter.
				token :=  currentChar isLetter.
				token := ( currentChar isLetter) example.
				^ token contents' #tests ) #( 'displayName
				| nameStream string |
				string := self name.
				nameStream := WriteStream on: (String new: 64).
					nameStream nextPutAll: string;
					nextPutAll: '' (''.
				self problemCount printOn: nameStream.
				nameStream nextPut: $).
				^nameStream contents' #tests ) ) ) #( #MyClassB #( #( 'someMethod
				^ instVarName2 := 3' #tests ) #( 'exampleMethod
				^ (currentChar isLetter) not' #tests ) #( 'dummyMethod
				(3 > self someMethod) not' #tests ) #( 'anotherMethod
				|aStream|
				aStream := WriteStream on: (String new: 128).
				aStream nextPutAll: ''dfgdf'';
					nextPutAll: '' (''.
				self problemCount printOn: aStream.
				aStream nextPut: $).' #tests ) ) ) #( #MyClassC #( #( 'methodWithArg: anArg
				(currentChar isLetter and: [anArg isDecimal])
					ifTrue: [^ self].
				^ nil' #tests ) #( 'visitMonospace: aFormat
				aFormat children do: [ :each | each accept: self ].
				self fixBlockWithoutMicText: aFormat' #tests ) #( 'visitStrike: aFormat
				aFormat children do: [ :each | each accept: self ].
				self fixBlockWithoutMicText: aFormat.
				#( 4 5 2 ) do: [ :val | val children do: [ :each | each accept: self ] ]' #tests ) #( 'visitParagraph: aParagraph
				^ aParagraph children do: [:each | each accept: self ]' #tests ) ) ) )
		in: newModel.

	^ newModel
]

{ #category : 'mocking' }
RBAbstractEnvTestCase >> rbModelForVariableTest [

	| newModel |
	
	newModel := self modelWithoutRealClasses: #( Foo Bar ).
	self defineFooBarIn: newModel.

	^ newModel
]

{ #category : 'running' }
RBAbstractEnvTestCase >> setUp [
	super setUp.
	testingEnvironment := Smalltalk globals
]

{ #category : 'set up' }
RBAbstractEnvTestCase >> setupInlineExpressionFor: aRefactoring toReturn: aBoolean [
	| options |
	options := aRefactoring options copy.
	options at: #shouldNotCreateExtraBindings put: [:ref :string | aBoolean].
	aRefactoring options: options
]

{ #category : 'actions' }
RBAbstractEnvTestCase >> shouldFail: aRefactoring [

	self proceedThroughWarning: [
		self
			should: [ aRefactoring generateChanges ]
			raise: RBRefactoringError
	]
]

{ #category : 'actions' }
RBAbstractEnvTestCase >> shouldWarn: aRefactoring [
	self
		should: [ aRefactoring generateChanges ]
		raise: RBRefactoringWarning
]

{ #category : 'accessing' }
RBAbstractEnvTestCase >> testingEnvironment: anObject [
	testingEnvironment := anObject
]
