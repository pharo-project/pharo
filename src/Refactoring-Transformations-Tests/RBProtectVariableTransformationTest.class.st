Class {
	#name : 'RBProtectVariableTransformationTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'running' }
RBProtectVariableTransformationTest >> setUp [

	super setUp.
	model := self rbModelForVariableTest
]

{ #category : 'tests' }
RBProtectVariableTransformationTest >> testAccessorsAlreadyExist [

	| class |
	(RBProtectVariableTransformation
		 model: model
		 instanceVariable: 'instVarName1'
		 class: #Foo) generateChanges.

	class := model classNamed: #Foo.
	self
		assert: (class parseTreeForSelector: #bar)
		equals: (self parseMethod: 'bar
			"Add one to instVarName1"
			self instVarName11: self instVarName11 + 1').
	self
		assert: (class parseTreeForSelector: #instVarName11:)
		equals: (self parseMethod: 'instVarName11: anObject
			instVarName1 := anObject').
	self
		assert: (class parseTreeForSelector: #instVarName11)
		equals: (self parseMethod: 'instVarName11 ^instVarName1').

	self
		assert: ((model classNamed: #Bar) parseTreeForSelector: #foo)
		equals: (self parseMethod: 'foo
			self instVarName11: self instVarName11 + instVarName2 + ClassVarName1')
]

{ #category : 'tests' }
RBProtectVariableTransformationTest >> testClassVariable [

	| refactoring class |
	refactoring := RBProtectVariableTransformation
		               classVariable: 'RecursiveSelfRule'
		               class: #RBTransformationRuleTestData.
	refactoring generateChanges.

	class := (refactoring model classNamed: #RBTransformationRuleTestData)
		         classSide.
	self
		assert: (class parseTreeForSelector: #recursiveSelfRule)
		equals: (self parseMethod: 'recursiveSelfRule ^RecursiveSelfRule').
	self
		assert: (class parseTreeForSelector: #recursiveSelfRule:)
		equals: (self parseMethod:
				 'recursiveSelfRule: anObject RecursiveSelfRule := anObject').

	self
		assert: (class parseTreeForSelector: #nuke)
		equals: (self parseMethod: 'nuke self recursiveSelfRule: nil').
	self
		assert: (class parseTreeForSelector: #initializeAfterLoad1)
		equals: (self parseMethod: 'initializeAfterLoad1
				self recursiveSelfRule: self parseTreeSearcher.
				self recursiveSelfRule
					addMethodSearches: #(''`@methodName: `@args | `@temps | self `@methodName: `@args'' ''`@methodName: `@args | `@temps | ^self `@methodName: `@args'')
					-> [:aNode :answer | true]').
	self
		assert:
			((refactoring model classNamed: #RBTransformationRuleTestData)
				 parseTreeForSelector: #checkMethod:)
		equals: (self parseMethod: 'checkMethod: aSmalllintContext
				class := aSmalllintContext selectedClass.
				(rewriteRule executeTree: aSmalllintContext parseTree) ifTrue:
					[(self class recursiveSelfRule executeTree: rewriteRule tree initialAnswer: false)
						ifFalse:
							[builder compile: rewriteRule tree printString
								in: class
								classified: aSmalllintContext protocols]]')
]

{ #category : 'tests' }
RBProtectVariableTransformationTest >> testClassVariableInModel [

	| class |
	(RBProtectVariableTransformation
		 model: model
		 classVariable: 'ClassVarName1'
		 class: #Foo) generateChanges.

	class := (model classNamed: #Foo) classSide.
	self
		assert: (class parseTreeForSelector: #classVarName1)
		equals: (self parseMethod: 'classVarName1 ^ClassVarName1').
	self
		assert: (class parseTreeForSelector: #classVarName1:)
		equals:
		(self parseMethod:
			 'classVarName1: anObject ClassVarName1 := anObject').

	self
		assert: (class parseTreeForSelector: #foo)
		equals: (self parseMethod: 'foo
				^self classVarName1: self classVarName1 * self classVarName1 * self classVarName1').

	self
		assert: (class instanceSide parseTreeForSelector: #classVarName1)
		equals:
		(self parseMethod: 'classVarName1 ^self class classVarName1').
	self
		assert: (class instanceSide parseTreeForSelector: #classVarName1:)
		equals: (self parseMethod: 'classVarName1: anObject
				^self class classVarName1: anObject')

	"self assert: (class instanceSide parseTreeFor: #asdf)
			equals: (RBParser parseMethod: 'asdf
				^self classVarName1: (self class classVarName1: self class classVarName1 + 1)').
	self assert: ((model classNamed: #Bar) parseTreeFor: #foo)
			equals: (RBParser parseMethod: 'foo
				instVarName1 := instVarName1 + instVarName2 + self class classVarName1').
	self assert: ((model classNamed: #Bar) parseTreeFor: #foo)
			equals: (RBParser parseMethod: 'foo
				instVarName1 := instVarName1 + instVarName2 + self class classVarName1')"
]

{ #category : 'tests' }
RBProtectVariableTransformationTest >> testMetaclass [

	| class |
	class := model metaclassNamed: #Foo.
	class addInstanceVariable: 'foo'.
	class compile: 'zzz ^foo := foo + foo * 2' classified: #( #testing ).

	(RBProtectVariableTransformation
		 model: model
		 instanceVariable: 'foo'
		 class: class) generateChanges.

	self
		assert: (class parseTreeForSelector: #foo1)
		equals: (self parseMethod: 'foo1 ^foo').
	self
		assert: (class parseTreeForSelector: #foo:)
		equals: (self parseMethod: 'foo: anObject foo := anObject').

	self
		assert: (class parseTreeForSelector: #zzz)
		equals:
		(self parseMethod: 'zzz ^self foo: self foo1 + self foo1 * 2')
]

{ #category : 'tests' }
RBProtectVariableTransformationTest >> testMetaclassFailure [

	self shouldFail: (RBProtectVariableTransformation
			 classVariable: #RecursiveSelfRule
			 class: RBTransformationRuleTestData class)
]

{ #category : 'tests' }
RBProtectVariableTransformationTest >> testRefactoring [

	| refactoring class |
	refactoring := RBProtectVariableTransformation
		               instanceVariable: 'builder'
		               class: #RBTransformationRuleTestData.
	refactoring generateChanges.

	class := refactoring model classNamed: #RBTransformationRuleTestData.
	self
		assert: (class parseTreeForSelector: #builder)
		equals: (self parseMethod: 'builder ^builder').
	self
		assert: (class parseTreeForSelector: #builder:)
		equals: (self parseMethod: 'builder: anObject
	builder := anObject').
	self
		assert: (class parseTreeForSelector: #viewResults)
		equals: (self parseMethod: 'viewResults
		self builder inspect.
		self resetResult').
	self
		assert: (class parseTreeForSelector: #checkMethod:)
		equals: (self parseMethod: 'checkMethod: aSmalllintContext
	class := aSmalllintContext selectedClass.
	(rewriteRule executeTree: aSmalllintContext parseTree)
		ifTrue:
			[(RecursiveSelfRule executeTree: rewriteRule tree initialAnswer: false)
				ifFalse:
					[self builder
						compile: rewriteRule tree printString
						in: class
						classified: aSmalllintContext protocols]]')
]

{ #category : 'tests' }
RBProtectVariableTransformationTest >> testTransform [

	| transformation class |
	transformation := RBProtectVariableTransformation
		                  instanceVariable: 'class'
		                  class: #RBTransformationRuleTestData.
	transformation generateChanges.

	class := transformation model classNamed:
		         #RBTransformationRuleTestData.
	self assert: (class directlyDefinesLocalMethod: #class1).
	self assert: (class directlyDefinesLocalMethod: #class:).

	self
		assert: (class parseTreeForSelector: #superSends)
		equals: (self parseMethod: 'superSends
		| rule |
		rule := OCParseTreeRewriter new.
		rule addSearch: ''super `@message: ``@args''
			-> (
				[:aNode |
				(self class1 withAllSubclasses
					detect: [:each | each includesSelector: aNode selector]
					ifNone: [nil]) isNil]
						-> ''self `@message: ``@args'').
		self rewriteUsing: rule').

	self
		assert: (class parseTreeForSelector: #checkMethod:)
		equals: (self parseMethod: 'checkMethod: aSmalllintContext
		self class: aSmalllintContext selectedClass.
		(rewriteRule executeTree: aSmalllintContext parseTree) ifTrue:
			[(RecursiveSelfRule executeTree: rewriteRule tree initialAnswer: false) ifFalse:
				[builder
					compile: rewriteRule tree printString
					in: self class1
					classified: aSmalllintContext protocols]]')
]

{ #category : 'tests' }
RBProtectVariableTransformationTest >> testVariableDoesNotExist [

	self
		shouldFail: (RBProtectVariableTransformation
				 instanceVariable: 'foo'
				 class: #RBBasicLintRuleTestData);
		shouldFail: (RBProtectVariableTransformation
				 classVariable: #Foo
				 class: #RBBasicLintRuleTestData)
]

{ #category : 'tests' }
RBProtectVariableTransformationTest >> testVariableIsNotAccessed [

	| transformation class |
	transformation := RBProtectVariableTransformation
		                  instanceVariable: 'instVar'
		                  class: self changeMockClass name.

	class := transformation model classNamed: self changeMockClass name.
	self deny: (class directlyDefinesLocalMethod: #instVar).
	self deny: (class directlyDefinesLocalMethod: #instVar:).

	transformation generateChanges.
	self assert: transformation model changes changes size equals: 3.
	"the accessors and the method using the variables"
	self assert: (class directlyDefinesLocalMethod: #instVar).
	self assert: (class directlyDefinesLocalMethod: #instVar:)
]

{ #category : 'tests' }
RBProtectVariableTransformationTest >> testVariableNotDirectlyDefined [

	self
		shouldFail: (RBProtectVariableTransformation
				 instanceVariable: 'name'
				 class: #RBBasicLintRuleTestData);
		shouldFail: (RBProtectVariableTransformation
				 classVariable: #DependentsFields
				 class: #RBBasicLintRuleTestData);
		shouldFail: (RBProtectVariableTransformation
				 model: model
				 classVariable: 'ClassVarName2'
				 class: #Bar)
]

{ #category : 'tests' }
RBProtectVariableTransformationTest >> testWithAssignment [

	| refactoring class |
	refactoring := RBProtectVariableTransformation
		               model: model
		               instanceVariable: 'instVarName2'
		               class: #Foo.
	refactoring generateChanges.

	class := model classNamed: #Foo.
	self
		assert: (class parseTreeForSelector: #instVarName2:)
		equals:
		(self parseMethod: 'instVarName2: anObject instVarName2 := anObject').
	self
		assert: (class parseTreeForSelector: #instVarName2)
		equals: (self parseMethod: 'instVarName2 ^instVarName2').

	self
		assert: (class parseTreeForSelector: #foo)
		equals: (self parseMethod: 'foo ^self instVarName2: 3').
	self
		assert: ((model classNamed: #Bar) parseTreeForSelector: #foo)
		equals: (self parseMethod: 'foo
			instVarName1 := instVarName1 + self instVarName2 + ClassVarName1')
]
