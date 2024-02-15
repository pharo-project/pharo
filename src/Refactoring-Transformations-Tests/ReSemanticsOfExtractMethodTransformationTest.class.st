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
		  model: model;
		  extractSource: sourceToExtract
		  fromMethodSource: source
		  to: newSelector
		  in: class name
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
			slots: { #instVar };
			package: 'Refactoring-DataForTesting'].
	class := model classNamed: 'ReClassForTesting'
]

{ #category : 'tests' }
ReSemanticsOfExtractMethodTransformationTest >> testBeginningExpressionsOfASequenceGotExtracted [

	| transformation |
	transformation := self 
extractSource: 'self foo. self bar.' 
fromSource: 'm 
		self foo.
		self bar.
		self end'
withNewSelector: #extractedMethod.
	transformation generateChanges.
	
self 
		assert: (class parseTreeForSelector: #extractedMethod)
		equals: (self parseMethod: 'extractedMethod self foo. ^ self bar.').
		
	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm self extractedMethod. self end')
]

{ #category : 'tests' }
ReSemanticsOfExtractMethodTransformationTest >> testCompleteSequenceGotExtracted [

	| transformation |
	transformation := self 
extractSource: 'self foo. self bar. self end' 
fromSource: 'm 
		self foo.
		self bar.
		self end'
withNewSelector: #extractedMethod.
	transformation generateChanges.
	
self 
		assert: (class parseTreeForSelector: #extractedMethod)
		equals: (self parseMethod: 'extractedMethod self foo. self bar. ^ self end').
		
	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm self extractedMethod')
]

{ #category : 'tests' }
ReSemanticsOfExtractMethodTransformationTest >> testExtractClassFromAssignmentExpectExtracted [

	| transformation |
	transformation := self 
extractSource: 'ReClassForTesting' 
fromSource: 'm 
		| temp |
		temp := ReClassForTesting'
withNewSelector: #extractedMethod.
	transformation generateChanges.
	
self 
		assert: (class parseTreeForSelector: #extractedMethod)
		equals: (self parseMethod: 'extractedMethod 
					^ ReClassForTesting').
		
	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm 
				| temp | 
				temp := self extractedMethod')
]

{ #category : 'tests' }
ReSemanticsOfExtractMethodTransformationTest >> testExtractSequenceEndingWithReturnExpectExtracted [

	| transformation |
	transformation := self 
extractSource: 'self bar. ^ self end.' 
fromSource: 'm 
		self foo.
		self bar.
		^ self end'
withNewSelector: #extractedMethod.
	transformation generateChanges.
	
self 
		assert: (class parseTreeForSelector: #extractedMethod)
		equals: (self parseMethod: 'extractedMethod 
					self bar. 
					^ self end').
		
	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm 
				self foo. 
				^ self extractedMethod')
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
		equals: (self parseMethod: 'extractedMethod ^ self bar.').
		
	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm self foo. self extractedMethod. self end')
]

{ #category : 'tests' }
ReSemanticsOfExtractMethodTransformationTest >> testTempAssignmentAndReferenceAfterExtractedCodeExpectGotExtracted [

	| transformation |
	transformation := self 
extractSource: 'a := 2. self foo: a.' 
fromSource: 'm 
		| a |
		a := 2.
		self foo: a.
		self end: a'
withNewSelector: #extractedMethod.
	transformation generateChanges.
	
self 
		assert: (class parseTreeForSelector: #extractedMethod)
		equals: (self parseMethod: 'extractedMethod 
					| a |
					a := 2. 
					self foo: a.
					^ a').
		
	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm
				| a |
				a := self extractedMethod.
				self end: a')
]

{ #category : 'tests' }
ReSemanticsOfExtractMethodTransformationTest >> testTempAssignmentAndReferenceExpectGotExtracted [

	| transformation |
	transformation := self 
extractSource: 'a := 2. self foo: a' 
fromSource: 'm 
		| a |
		a := 2.
		self foo: a'
withNewSelector: #extractedMethod.
	transformation generateChanges.
	
self 
		assert: (class parseTreeForSelector: #extractedMethod)
		equals: (self parseMethod: 'extractedMethod 
					| a |
					a := 2. 
					^ self foo: a').
		
	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm
				self extractedMethod')
]

{ #category : 'tests' }
ReSemanticsOfExtractMethodTransformationTest >> testTempAssignmentAndReturnWithTempReferenceExpectGotExtracted [

	| transformation |
	transformation := self 
extractSource: 'a := 2. ^ self foo: a.' 
fromSource: 'm 
		| a |
		a := 2.
		^ self foo: a'
withNewSelector: #extractedMethod.
	transformation generateChanges.
	
self 
		assert: (class parseTreeForSelector: #extractedMethod)
		equals: (self parseMethod: 'extractedMethod 
					| a |
					a := 2. 
					^ self foo: a').
		
	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm
				^ self extractedMethod')
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
					^ self end').
		
	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm 
				self foo. 
				self extractedMethod')
]
