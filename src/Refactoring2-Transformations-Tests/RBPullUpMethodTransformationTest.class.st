Class {
	#name : #RBPullUpMethodTransformationTest,
	#superclass : #RBParametrizedTransformationTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #initialization }
RBPullUpMethodTransformationTest >> addClassHierarchy [
	model 
		defineClass: 'Object subclass: #SomeClass instanceVariableNames: '''' classVariableNames: ''Foo'' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model 
		defineClass: 'SomeClass subclass: #Subclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model 
		defineClass: 'Subclass subclass: #Foo1 instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model 
		defineClass: 'Subclass subclass: #Foo2 instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
]

{ #category : #initialization }
RBPullUpMethodTransformationTest >> addClassHierarchyForPools [
	model 
		defineClass: 'SharedPool subclass: #TotoSharedPool instanceVariableNames: '''' classVariableNames: ''SP'' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model 
		defineClass: 'Object subclass: #TotoSuperclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model 
		defineClass: 'TotoSuperclass subclass: #Toto instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: ''TotoSharedPool'' category: #''Refactory-Test data'''.
	(model classNamed: #Toto) compile: 'poolReference ^ SP' classified: #(#accessing).
]

{ #category : #'failure tests' }
RBPullUpMethodTransformationTest >> testDoesntPullUpReferencesInstVar [
	| refactoring class |
	class := model classNamed: ('RBTransformation', 'RuleTestData1') asSymbol.
	refactoring := RBPullUpMethodTransformation
			model: model
			pullUp: #(#foo )
			from: class.
	[ self shouldFail: refactoring asRefactoring ] valueSupplyingAnswer: false.
]

{ #category : #tests }
RBPullUpMethodTransformationTest >> testPullUpAndCopyDown [
	| class |
	self addClassHierarchy.
	class := model classNamed: #Foo1.
	class compile: 'yourself ^1' classified: #(#accessing).
	self proceedThroughWarning: [ (RBPullUpMethodTransformation
				model: model
				pullUp: #(#yourself)
				from: class) asRefactoring transform ].
	self assert: (class superclass parseTreeFor: #yourself) 
			equals: (self parseMethod: 'yourself ^1').
	self deny: (class directlyDefinesMethod: #yourself).
	class := model classNamed: #Foo2.
	self assert: (class directlyDefinesMethod: #yourself).
	self assert: (class parseTreeFor: #yourself) 
			equals: ((model classNamed: #Object) parseTreeFor: #yourself)
]

{ #category : #'failure tests' }
RBPullUpMethodTransformationTest >> testPullUpClassMethod [
	| class |
	class := model classNamed: #Object.
	class addClassVariable: #Foo.
	class classSide 
		compile: 'foo ^Foo'
		classified: #(#accessing ).
	self shouldFail: (RBPullUpMethodTransformation
			model: model
			pullUp: #(#foo )
			from: class classSide) asRefactoring
]

{ #category : #tests }
RBPullUpMethodTransformationTest >> testPullUpInAHighHierarchyClass [
	| class superClass |
	
	self addClassHierarchy .
	class := model classNamed: #Foo1.
	superClass := model classNamed: #SomeClass .
	class compile: 'example ^1' classified: #(#accessing).
	(RBPullUpMethodTransformation
				model: model
				pullUp: #(#example)
				from: class 
				to: superClass ) asRefactoring transform.
	self assert: (superClass parseTreeFor: #example) 
				equals: (self parseMethod: 'example ^1').
	self deny: (class directlyDefinesMethod: #example).
	class := model classNamed: #Subclass.
	self assert: (class definesMethod: #example).
	self assert: (class parseTreeFor: #example) 
				equals: ((model classNamed: #SomeClass) parseTreeFor: #example)
]

{ #category : #tests }
RBPullUpMethodTransformationTest >> testPullUpMethodWithCopyOverriddenMethodsDown [
	| refactoring |
	self proceedThroughWarning: 
		[ refactoring := RBPullUpMethodTransformation
			pullUp: #(#isComposite )
			from: RBCompositeLintRuleTestData.
		refactoring asRefactoring transform ].
	self assert: ((refactoring model classNamed: #RBBasicLintRuleTestData) parseTreeFor: #isComposite) equals: (self parseMethod: 'isComposite ^false').
	self assert: ((refactoring model classNamed: ('RBFoo' , 'LintRuleTestData') asSymbol) parseTreeFor: #isComposite) equals: (self parseMethod: 'isComposite ^false').
	self assert: ((refactoring model classNamed: #RBLintRuleTestData) parseTreeFor: #isComposite) equals: (self parseMethod: 'isComposite ^true').
	self deny: ((refactoring model classNamed: #RBCompositeLintRuleTestData) directlyDefinesMethod: #isComposite)
]

{ #category : #tests }
RBPullUpMethodTransformationTest >> testPullUpMethodWithSharedPool [
	| class superClass |
	
	self addClassHierarchyForPools.
	class := model classNamed: #Toto.
	superClass := model classNamed: #TotoSuperclass.
	self proceedThroughWarning: [ (RBPullUpMethodTransformation 
				model: model
				pullUp: #(#poolReference)
				from: class 
				to: superClass)asRefactoring transform ].
	self assert: (superClass parseTreeFor: #poolReference) 
				equals: (self parseMethod: 'poolReference ^ SP').
	self deny: (class directlyDefinesMethod: #poolReference).
]

{ #category : #tests }
RBPullUpMethodTransformationTest >> testPullUpReferencesInstVar [
	| refactoring class superClass |
	class := model classNamed: ('RBTransformation', 'RuleTestData1') asSymbol.
	refactoring := RBPullUpMethodTransformation
			model: model
			pullUp: #(#foo )
			from: class.
	[ refactoring asRefactoring transform ] valueSupplyingAnswer: true.
	superClass := model classNamed: ('RBFooLint', 'RuleTestData1') asSymbol.
	self assert: (superClass parseTreeFor: #foo) 
				equals: (self parseMethod: 'foo ^ foo').
	self assert: (superClass directlyDefinesInstanceVariable: #foo).
	self deny: (class directlyDefinesMethod: #foo).
	self deny: (class directlyDefinesInstanceVariable: #foo).
]

{ #category : #'failure tests' }
RBPullUpMethodTransformationTest >> testPullUpWhenSuperClassDoesNotDirectlyImplement [
	| classEnvironment classes |
	classes := #(#ClassA #ClassB #ClassC)
		inject: OrderedCollection new
		into: [ :sum :each | 
			testingEnvironment
				at: each
				ifPresent: [ :class | 
					sum
						add: class;
						add: class class ].
			sum ].
	classEnvironment := RBClassEnvironment classes: classes.
	model name: 'Test'.
	"Classes"
	#('Object subclass: #ClassA
		instanceVariableNames: ''''
		classVariableNames: ''''
		poolDictionaries: ''''
		category: ''Testing'' ' 'ClassA subclass: #ClassB
		instanceVariableNames: ''''
		classVariableNames: ''''
		poolDictionaries: ''''
		category: ''Testing'' ' 'ClassB subclass: #ClassC
		instanceVariableNames: ''''
		classVariableNames: ''''
		poolDictionaries: ''''
		category: ''Testing'' ') do: [ :each | model defineClass: each ].
	#(#(#ClassA #(#('foo
			^ ''ClassA foo''' #private))) #(#ClassC #(#('foo
			^ ''ClassC foo''' #private))))
		do: [ :each | 
			| class |
			class := model classNamed: each first.
			each last
				do:
					[ :methodPair | class compile: methodPair first classified: methodPair last ] ].
	self
		shouldntWarn:
			 (RBPullUpMethodTransformation
				model: model
				pullUp: #(#foo)
				from: (model classNamed: #ClassC)) asRefactoring
]

{ #category : #'failure tests' }
RBPullUpMethodTransformationTest >> testPullUpWithInvalidSuperSend [
	| class |
	self addClassHierarchy .
	(model classNamed: #Foo2) compile: 'yourself ^super yourself + 1'
		classified: #(#accessing).
	class := model classNamed: #Foo1.
	class compile: 'yourself ^1' classified: #(#accessing).
	self shouldFail: (RBPullUpMethodTransformation
				model: model
				pullUp: #(#yourself)
				from: class) asRefactoring
]

{ #category : #'failure tests' }
RBPullUpMethodTransformationTest >> testPullUpWithMethodThatCannotBePullUp [
	model 
		defineClass: 'Object subclass: #SomeClass instanceVariableNames: '''' classVariableNames: ''Foo'' poolDictionaries: '''' category: #''Refactory-Test data'''.
	(model classNamed: #SomeClass) compile: 'yourself ^54'
		classified: #(#accessing).
	self shouldFail: (RBPullUpMethodTransformation 
				model: model
				pullUp: #(#yourself)
				from: (model classNamed: #SomeClass)) asRefactoring
]

{ #category : #'failure tests' }
RBPullUpMethodTransformationTest >> testPullUpWithSuperSendThatCannotBeCopiedDown [
	| class |
	self addClassHierarchy.
	(model classNamed: #Object) compile: 'foo ^3' classified: #(#accessing).
	(model classNamed: #Subclass) compile: 'foo ^super foo'
		classified: #(#accessing).
	(class := model classNamed: #Foo2) compile: 'foo ^1'
		classified: #(#accessing).
	self shouldFail: (RBPullUpMethodTransformation
				model: model
				pullUp: #(#foo)
				from: class) asRefactoring
]
