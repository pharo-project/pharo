Class {
	#name : 'ReSemanticsOfExtractMethodTransformationTest',
	#superclass : 'RBAbstractTransformationTest',
	#instVars : [
		'class'
	],
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'private - testing' }
ReSemanticsOfExtractMethodTransformationTest >> extractSource: sourceToExtract fromSource: source withNewSelector: newSelector [

	class compile: source classified: '#test data'.

	^ RBExtractMethodTransformation new
		  extractSource: sourceToExtract
		  fromMethodSource: source
		  to: newSelector
		  in: class
]

{ #category : 'set up' }
ReSemanticsOfExtractMethodTransformationTest >> setUp [

	| package |
	super setUp.
	package := RBPackageEnvironment packageName: 'Refactoring-DataForTesting'.
	model := RBNamespace onEnvironment: package.
	model defineClass: [ :aBuilder | 
		aBuilder
			superclass: Object;
			name: #ReClassForTesting;
			package: 'Refactoring-DataForTesting'].
	class := model classNamed: 'ReClassForTesting'
]

{ #category : 'tests' }
ReSemanticsOfExtractMethodTransformationTest >> testMiddleExpressionsOfASequenceGotExtracted [

	| transformation |
	transformation := self 
extractSource: 'self bar.' 
fromSource: 'm 
		self foo.
		self bar.
		self end'
withNewSelector: #extractedMethod.
	transformation generateChanges.
	
self 
		assert: (class parseTreeForSelector: #extractedMethod)
		equals: (self parseMethod: 'extractedMethod self bar.').
		
	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm self foo. self extractedMethod. self end')
]

{ #category : 'tests' }
ReSemanticsOfExtractMethodTransformationTest >> testTwoLastExpressionsOfASequenceGotExtracted [

	| transformation |
	transformation := self 
extractSource: 'self bar. self end.' 
fromSource: 'm 
		self foo.
		self bar.
		self end'
withNewSelector: #extractedMethod.
	transformation generateChanges.
	
self 
		assert: (class parseTreeForSelector: #extractedMethod)
		equals: (self parseMethod: 'extractedMethod 
					self bar. 
					self end').
		
	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm 
				self foo. 
				self extractedMethod')
]
