Class {
	#name : 'RBPullUpMethodParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-Parametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'building suites' }
RBPullUpMethodParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RePullUpMethodRefactoring };
		addCase: { #rbClass -> RBPullUpMethodTransformation };
		yourself
]

{ #category : 'tests' }
RBPullUpMethodParametrizedTest >> addClassHierarchy [

	model
		defineClass: [ :aBuilder |
			aBuilder
				superclass: Object;
				name: #SomeClass;
				sharedVariables: { #Foo };
				package: #'Refactory-Test data' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #SomeClass;
				name: #Subclass;
				package: #'Refactory-Test data' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #Subclass;
				name: #Foo1;
				package: #'Refactory-Test data' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #Subclass;
				name: #Foo2;
				package: #'Refactory-Test data' ]
]

{ #category : 'tests' }
RBPullUpMethodParametrizedTest >> addClassHierarchyForPools [

	model
		defineClass: [ :aBuilder |
			aBuilder
				superclass: SharedPool;
				name: #TotoSharedPool;
				sharedVariables: { #SP };
				package: #'Refactory-Test data' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclass: Object;
				name: #TotoSuperclass;
				package: #'Refactory-Test data' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #TotoSuperclass;
				name: #Toto;
				sharedPools: #(TotoSharedPool);
				package: #'Refactory-Test data' ].

	(model classNamed: #Toto) compile: 'poolReference ^ SP' classified: #( #accessing )
]

{ #category : 'accessing' }
RBPullUpMethodParametrizedTest >> constructor [
	^ #pullUp:from:
]

{ #category : 'failure tests' }
RBPullUpMethodParametrizedTest >> testFailurePullUpClassMethod [

	| class |
	class := model classNamed: #Object.
	class addClassVariable: #Foo.
	class classSide compile: 'foo ^Foo' classified: #( #accessing ).
	self shouldFail:
		(self createRefactoringWithModel: model andArguments: {
				 #( #foo ).
				 class classSide })
]

{ #category : 'failure tests' }
RBPullUpMethodParametrizedTest >> testFailurePullUpWhenSuperClassDoesNotDirectlyImplement [

	| classEnvironment classes |
	classes := #( #ClassA #ClassB #ClassC ) inject: OrderedCollection new into: [ :sum :each |
		           testingEnvironment at: each ifPresent: [ :class |
			           sum
				           add: class;
				           add: class class ].
		           sum ].
	classEnvironment := RBClassEnvironment classes: classes.
	model name: 'Test'.
	"Classes"
	model
		defineClass: [ :aBuilder |
			aBuilder
				superclass: Object;
				name: #ClassA;
				package: 'Testing' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #ClassA;
				name: #ClassB;
				package: 'Testing' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #ClassB;
				name: #ClassC;
				package: 'Testing' ].
	#( #( #ClassA #( #( 'foo
			^ ''ClassA foo''' #private ) ) ) #( #ClassC #( #( 'foo
			^ ''ClassC foo''' #private ) ) ) ) do: [ :each |
		| class |
		class := model classNamed: each first.
		each last do: [ :methodPair | class compile: methodPair first classified: methodPair last ] ].
	self shouldntWarn: (self createRefactoringWithModel: model andArguments: {
				 #( #foo ).
				 (model classNamed: #ClassC) })
]

{ #category : 'failure tests' }
RBPullUpMethodParametrizedTest >> testFailurePullUpWithInvalidSuperSend [

	| class |
	self addClassHierarchy.
	(model classNamed: #Foo2)
		compile: 'yourself ^super yourself + 1'
		classified: #( #accessing ).
	class := model classNamed: #Foo1.
	class compile: 'yourself ^1' classified: #( #accessing ).

	self shouldFail:
		(self createRefactoringWithModel: model andArguments: {
				 #( #yourself ).
				 class })
]

{ #category : 'failure tests' }
RBPullUpMethodParametrizedTest >> testFailurePullUpWithMethodThatCannotBePullUp [

	model defineClass: [ :aBuilder |
		aBuilder
			superclass: Object;
			name: #SomeClass;
			sharedVariables: { #Foo };
			package: #'Refactory-Test data' ].
	(model classNamed: #SomeClass) compile: 'yourself ^54' classified: #( #accessing ).

	self shouldFail: (self createRefactoringWithModel: model andArguments: {
				 #( #yourself ).
				 (model classNamed: #SomeClass) })
]

{ #category : 'failure tests' }
RBPullUpMethodParametrizedTest >> testFailurePullUpWithSuperSendThatCannotBeCopiedDown [

	| class |
	self addClassHierarchy.
	(model classNamed: #Object)
		compile: 'foo ^3'
		classified: #( #accessing ).
	(model classNamed: #Subclass)
		compile: 'foo ^super foo'
		classified: #( #accessing ).
	(class := model classNamed: #Foo2)
		compile: 'foo ^1'
		classified: #( #accessing ).

	self shouldFail:
		(self createRefactoringWithModel: model andArguments: {
				 #( #foo ).
				 class })
]

{ #category : 'tests' }
RBPullUpMethodParametrizedTest >> testPullUpAndCopyDown [
	| class |
	self addClassHierarchy.
	class := model classNamed: #Foo1.
	class compile: 'yourself ^1' classified: #(#accessing).
	self proceedThroughWarning: [
		self executeRefactoring: (self createRefactoringWithModel: model
			andArguments: { #(#yourself) . class }) ].
	self assert: (class superclass parseTreeForSelector: #yourself)
			equals: (self parseMethod: 'yourself ^1').
	self deny: (class directlyDefinesMethod: #yourself).
	class := model classNamed: #Foo2.
	self assert: (class directlyDefinesMethod: #yourself).
	self assert: (class parseTreeForSelector: #yourself)
			equals: ((model classNamed: #Object) parseTreeForSelector: #yourself)
]

{ #category : 'tests' }
RBPullUpMethodParametrizedTest >> testPullUpInAHighHierarchyClass [
	| class superClass |

	self addClassHierarchy .
	class := model classNamed: #Foo1.
	self addClassHierarchy.
	superClass := model classNamed: #SomeClass .
	class compile: 'example ^1' classified: #(#accessing).
	self executeRefactoring: (rbClass
				model: model
				pullUp: #(#example)
				from: class
				to: superClass ).
	self assert: (superClass parseTreeForSelector: #example)
				equals: (self parseMethod: 'example ^1').
	self deny: (class directlyDefinesMethod: #example).
	class := model classNamed: #Subclass.
	self assert: (class definesMethod: #example).
	self assert: (class parseTreeForSelector: #example)
				equals: ((model classNamed: #SomeClass) parseTreeForSelector: #example)
]

{ #category : 'tests' }
RBPullUpMethodParametrizedTest >> testPullUpMethodWithCopyOverriddenMethodsDown [
	| refactoring |
	self proceedThroughWarning:
		[ refactoring := self createRefactoringWithArguments:
			{ #(#isComposite ) . RBCompositeLintRuleTestData }.
		self executeRefactoring: refactoring ].
	self assert: ((refactoring model classNamed: #RBBasicLintRuleTestData)
			parseTreeForSelector: #isComposite)
		equals: (self parseMethod: 'isComposite ^false').
	self assert: ((refactoring model classNamed: ('RBFoo' , 'LintRuleTestData') asSymbol)
			parseTreeForSelector: #isComposite)
		equals: (self parseMethod: 'isComposite ^false').
	self assert: ((refactoring model classNamed: #RBLintRuleTestData)
			parseTreeForSelector: #isComposite)
		equals: (self parseMethod: 'isComposite ^true').
	self deny: ((refactoring model classNamed: #RBCompositeLintRuleTestData)
		directlyDefinesMethod: #isComposite)
]

{ #category : 'tests' }
RBPullUpMethodParametrizedTest >> testPullUpMethodWithSharedPool [
	| class superClass |

	self addClassHierarchyForPools.
	class := model classNamed: #Toto.
	superClass := model classNamed: #TotoSuperclass.
	self proceedThroughWarning: [
		self executeRefactoring: (rbClass
			model: model
			pullUp: #(#poolReference)
			from: class
			to: superClass) ].
	self assert: (superClass parseTreeForSelector: #poolReference)
				equals: (self parseMethod: 'poolReference ^ SP').
	self deny: (class directlyDefinesMethod: #poolReference)
]

{ #category : 'tests' }
RBPullUpMethodParametrizedTest >> testPullUpReferencesInstVar [
	| refactoring class superClass |
	class := model classNamed: ('RBTransformation', 'RuleTestData1') asSymbol.
	refactoring := self createRefactoringWithModel: model
		andArguments: { #(#foo ) . class }.
	self proceedThroughWarning: [ self executeRefactoring: refactoring  ].
	superClass := model classNamed: ('RBFooLint', 'RuleTestData1') asSymbol.
	self assert: (superClass parseTreeForSelector: #foo)
				equals: (self parseMethod: 'foo ^ foo').
	self deny: (class directlyDefinesMethod: #foo).
	self assert: (class directlyDefinesInstanceVariable: #foo)
]

{ #category : 'failure tests' }
RBPullUpMethodParametrizedTest >> testWarnDoesntPullUpReferencesInstVar [

	| refactoring class |
	rbClass = RBPullUpMethodTransformation ifTrue: [ " RBPullUpMethodTransformation shouldn't warn when instance variables are referenced "
		self skip ].
	
	class := model classNamed:
		         ('RBTransformation' , 'RuleTestData1') asSymbol.
	refactoring := self
		               createRefactoringWithModel: model
		               andArguments: {
				               #( #foo ).
				               class }.
	[ self shouldWarn: refactoring ] valueSupplyingAnswer: false
]
