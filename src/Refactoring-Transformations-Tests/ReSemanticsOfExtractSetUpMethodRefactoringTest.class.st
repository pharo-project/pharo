Class {
	#name : 'ReSemanticsOfExtractSetUpMethodRefactoringTest',
	#superclass : 'RBAbstractTransformationTest',
	#instVars : [
		'class'
	],
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'as yet unclassified' }
ReSemanticsOfExtractSetUpMethodRefactoringTest >> extractSource: sourceToExtract fromSource: source [

	| method |
	method := class compile: source classified: '#test data'.

	^ ReCompositeExtractSetUpMethodRefactoring new
		  model: model;
		  extractSetUpCode: sourceToExtract
		  fromMethod: method selector
		  inClass: class name
]

{ #category : 'running' }
ReSemanticsOfExtractSetUpMethodRefactoringTest >> setUp [

	| package |
	super setUp.
	package := RBPackageEnvironment packageName: TestCase package name.
	model := RBNamespace onEnvironment: package.
	model defineClass: [ :aBuilder | 
		aBuilder
			superclass: TestCase;
			name: #ReClassForTesting;
			slots: { #instVar . #instVar2 };
			package: 'Refactoring-DataForTesting'].
	class := model classNamed: 'ReClassForTesting'
]

{ #category : 'tests' }
ReSemanticsOfExtractSetUpMethodRefactoringTest >> testExtractSetUpWhenSingleMessageSendSelectedExpectExtracted [

		| refactoring |
	refactoring := self 
extractSource: 'self localSetUp' 
fromSource: 'testSomething
		self localSetUp.
		instVar2 := instVar + 1.
		self assert: instVar2 > instVar'.

	refactoring generateChanges.
	
	self 
		assert: (class parseTreeForSelector: #setUp)
		equals: (self parseMethod: 'setUp
					super setUp.
					self localSetUp').
		
	self 
		assert: (class parseTreeForSelector: #testSomething)
		equals: (self parseMethod: 'testSomething
		instVar2 := instVar + 1.
		self assert: instVar2 > instVar')
]
