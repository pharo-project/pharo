Class {
	#name : #RBProtectVariableTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #running }
RBProtectVariableTransformationTest >> setUp [

	super setUp.
	model := self abstractVariableTestData.
]

{ #category : #testing }
RBProtectVariableTransformationTest >> testAccessorsAlreadyExist [

	| class |
	(RBProtectVariableTransformation
		model: model
		instanceVariable: 'instVarName1'
		class: #Foo)
		asRefactoring transform.
	
	class := model classNamed: #Foo.
	self assert: (class parseTreeFor: #bar)
		equals: (RBParser parseMethod: 'bar
			"Add one to instVarName1"
			self instVarName11: self instVarName11 + 1').
	self assert: (class parseTreeFor: #instVarName11:) 
		equals: (RBParser parseMethod: 'instVarName11: anObject
			instVarName1 := anObject').
	self assert: (class parseTreeFor: #instVarName11) 
		equals: (RBParser parseMethod: 'instVarName11 ^instVarName1').
		
	self assert: ((model classNamed: #Bar) parseTreeFor: #foo) 
		equals: (RBParser parseMethod: 'foo
			self instVarName11: self instVarName11 + instVarName2 + ClassVarName1')
]

{ #category : #testing }
RBProtectVariableTransformationTest >> testClassVariable [

	| refactoring class |
	refactoring := (RBProtectVariableTransformation 
						classVariable: 'RecursiveSelfRule'
						class: #RBTransformationRuleTest)
						asRefactoring transform.
	
	class := (refactoring model classNamed: #RBTransformationRuleTest) theMetaClass.
	self assert: (class parseTreeFor: #recursiveSelfRule)
			equals: (RBParser parseMethod: 'recursiveSelfRule ^RecursiveSelfRule').
	self assert: (class parseTreeFor: #recursiveSelfRule:) 
			equals: (RBParser parseMethod: 'recursiveSelfRule: anObject RecursiveSelfRule := anObject').
	
	self assert: (class parseTreeFor: #nuke) 
			equals: (RBParser parseMethod: 'nuke self recursiveSelfRule: nil').
	self assert: (class parseTreeFor: #initializeAfterLoad1) 
			equals: (RBParser parseMethod: 'initializeAfterLoad1
				self recursiveSelfRule: RBParseTreeSearcher new.
				self recursiveSelfRule
					addMethodSearches: #(''`@methodName: `@args | `@temps | self `@methodName: `@args'' ''`@methodName: `@args | `@temps | ^self `@methodName: `@args'')
					-> [:aNode :answer | true]').
	self assert: ((refactoring model classNamed: #RBTransformationRuleTest) parseTreeFor: #checkMethod:) 
			equals: (RBParser parseMethod: 'checkMethod: aSmalllintContext 
				class := aSmalllintContext selectedClass.
				(rewriteRule executeTree: aSmalllintContext parseTree) ifTrue: 
					[(self class recursiveSelfRule executeTree: rewriteRule tree initialAnswer: false)
						ifFalse: 
							[builder compile: rewriteRule tree printString
								in: class
								classified: aSmalllintContext protocols]]')
]

{ #category : #testing }
RBProtectVariableTransformationTest >> testClassVariableInModel [

	| class |
	(RBProtectVariableTransformation 
		model: model
		classVariable: 'ClassVarName1'
		class: #Foo)
		asRefactoring transform.
	
	class := (model classNamed: #Foo) theMetaClass.
	self assert: (class parseTreeFor: #classVarName1) 
			equals: (RBParser parseMethod: 'classVarName1 ^ClassVarName1').
	self assert: (class parseTreeFor: #classVarName1:) 
			equals: (RBParser parseMethod: 'classVarName1: anObject ClassVarName1 := anObject').
	
	self assert: (class parseTreeFor: #foo)
			equals: (RBParser parseMethod: 'foo
				^self classVarName1: self classVarName1 * self classVarName1 * self classVarName1').
					
	self assert: (class theNonMetaClass parseTreeFor: #classVarName1)
			equals: (RBParser parseMethod: 'classVarName1 ^self class classVarName1').
	self assert: (class theNonMetaClass parseTreeFor: #classVarName1:) 
			equals: (RBParser parseMethod: 'classVarName1: anObject
				^self class classVarName1: anObject').
				
	"self assert: (class theNonMetaClass parseTreeFor: #asdf)
			equals: (RBParser parseMethod: 'asdf
				^self classVarName1: (self class classVarName1: self class classVarName1 + 1)').
	self assert: ((model classNamed: #Bar) parseTreeFor: #foo)
			equals: (RBParser parseMethod: 'foo
				instVarName1 := instVarName1 + instVarName2 + self class classVarName1').
	self assert: ((model classNamed: #Bar) parseTreeFor: #foo)
			equals: (RBParser parseMethod: 'foo
				instVarName1 := instVarName1 + instVarName2 + self class classVarName1')"
]

{ #category : #testing }
RBProtectVariableTransformationTest >> testMetaclass [

	| class |
	class := model metaclassNamed: #Foo.
	class addInstanceVariable: 'foo'.
	class compile: 'zzz ^foo := foo + foo * 2' classified: #(#testing).
	
	(RBProtectVariableTransformation
		model: model
		instanceVariable: 'foo'
		class: class)
		asRefactoring transform.
				
	self assert: (class parseTreeFor: #foo1) 
			equals: (RBParser parseMethod: 'foo1 ^foo').
	self assert: (class parseTreeFor: #foo:)
			equals: (RBParser parseMethod: 'foo: anObject foo := anObject').
			
	self assert: (class parseTreeFor: #zzz) 
			equals: (RBParser parseMethod: 'zzz ^self foo: self foo1 + self foo1 * 2')
]

{ #category : #testing }
RBProtectVariableTransformationTest >> testMetaclassFailure [

	self shouldFail: (RBProtectVariableTransformation 
							classVariable: #RecursiveSelfRule
							class: RBTransformationRuleTest class)
							asRefactoring
]

{ #category : #testing }
RBProtectVariableTransformationTest >> testRefactoring [

	| refactoring class |
	refactoring := (RBProtectVariableTransformation
						instanceVariable: 'builder'
						class: #RBTransformationRuleTest)
						asRefactoring transform.
	
	class := refactoring model classNamed: #RBTransformationRuleTest.
	self assert: (class parseTreeFor: #builder) equals: (RBParser parseMethod: 'builder ^builder').
	self assert: (class parseTreeFor: #builder:) equals: (RBParser parseMethod: 'builder: anObject
	builder := anObject').
	self assert: (class parseTreeFor: #viewResults) equals: (RBParser parseMethod: 'viewResults
		self builder inspect.
		self resetResult').
	self assert: (class parseTreeFor: #checkMethod:) equals: (RBParser parseMethod: 'checkMethod: aSmalllintContext 
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

{ #category : #testing }
RBProtectVariableTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBProtectVariableTransformation
							instanceVariable: 'class'
							class: #RBTransformationRuleTest)
							transform.
		
	class := transformation model classNamed: #RBTransformationRuleTest.
	self assert: (class directlyDefinesLocalMethod: #class1).
	self assert: (class directlyDefinesLocalMethod: #class:).
	
	self assert: (class parseTreeFor: #superSends) equals: (RBParser parseMethod: 
	'superSends
		| rule |
		rule := RBParseTreeRewriter new.
		rule addSearch: ''super `@message: ``@args'' 
			-> (
				[:aNode | 
				(self class1 withAllSubclasses 
					detect: [:each | each includesSelector: aNode selector]
					ifNone: [nil]) isNil] 
						-> ''self `@message: ``@args'').
		self rewriteUsing: rule').
	
	self assert: (class parseTreeFor: #checkMethod:) equals: (RBParser parseMethod:
	'checkMethod: aSmalllintContext 
		self class: aSmalllintContext selectedClass.
		(rewriteRule executeTree: aSmalllintContext parseTree) ifTrue: 
			[(RecursiveSelfRule executeTree: rewriteRule tree initialAnswer: false) ifFalse: 
				[builder 
					compile: rewriteRule tree printString
					in: self class1
					classified: aSmalllintContext protocols]]')
]

{ #category : #testing }
RBProtectVariableTransformationTest >> testVariableDoesNotExist [

	self
		shouldFail: (RBProtectVariableTransformation 
						instanceVariable: 'foo'
						class: #RBBasicLintRuleTest)
						asRefactoring;
		shouldFail: (RBProtectVariableTransformation 
						classVariable: #Foo
						class: #RBBasicLintRuleTest)
						asRefactoring
]

{ #category : #testing }
RBProtectVariableTransformationTest >> testVariableIsNotAccessed [

	| transformation class |
	transformation := RBProtectVariableTransformation 
							instanceVariable: 'instVar'
							class: self changeMock name.
	
	class := transformation model classNamed: self changeMock name asSymbol.
	self deny: (class directlyDefinesLocalMethod: #instVar).
	self deny: (class directlyDefinesLocalMethod: #instVar:).
	
	transformation transform.	
	self assert: transformation model changes changes size equals: 2.
	self assert: (class directlyDefinesLocalMethod: #instVar).
	self assert: (class directlyDefinesLocalMethod: #instVar:).
]

{ #category : #testing }
RBProtectVariableTransformationTest >> testVariableNotDirectlyDefined [

	self 
		shouldFail: (RBProtectVariableTransformation 
						instanceVariable: 'name'
						class: #RBBasicLintRuleTest)
						asRefactoring;
		shouldFail: (RBProtectVariableTransformation 
						classVariable: #DependentsFields
						class: #RBBasicLintRuleTest)
						asRefactoring;
		shouldFail: (RBProtectVariableTransformation 
						model: model
						classVariable: 'ClassVarName2'
						class: #Bar)
						asRefactoring
]

{ #category : #testing }
RBProtectVariableTransformationTest >> testWithAssignment [

	| refactoring class |
	refactoring := (RBProtectVariableTransformation
						model: model
						instanceVariable: 'instVarName2'
						class: #Foo)
						asRefactoring transform.
	
	class := model classNamed: #Foo.
	self assert: (class parseTreeFor: #instVarName2:)
		equals: (RBParser parseMethod: 'instVarName2: anObject instVarName2 := anObject').
	self assert: (class parseTreeFor: #instVarName2) 
		equals: (RBParser parseMethod: 'instVarName2 ^instVarName2').
	
	self assert: (class parseTreeFor: #foo)
		equals: (RBParser parseMethod: 'foo ^self instVarName2: 3').
	self assert: ((model classNamed: #Bar) parseTreeFor: #foo)
		equals: (RBParser parseMethod: 'foo
			instVarName1 := instVarName1 + self instVarName2 + ClassVarName1')
]
