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

	| method |
	method := class compile: source classified: '#test data'.

	^ RBExtractMethodTransformation new
		  model: model;
		  extract: sourceToExtract
		  from: method selector
		  to: newSelector
		  in: class name
]

{ #category : 'running' }
ReSemanticsOfExtractMethodTransformationTest >> setUp [

	| package |
	super setUp.
	package := RBPackageEnvironment packageName: 'Refactoring-DataForTesting'.
	model := RBNamespace onEnvironment: package.
	model defineClass: [ :aBuilder | 
		aBuilder
			superclass: Object;
			name: #ReClassForTesting;
			slots: { #instVar . #instVar2 };
			package: 'Refactoring-DataForTesting'].
	class := model classNamed: 'ReClassForTesting'
]

{ #category : 'tests' }
ReSemanticsOfExtractMethodTransformationTest >> testArgumentIsNotUsedInExtractedCodeExpectGotExtracted [

	| transformation |
	transformation := self 
extractSource: 'a := instVar + 1' 
fromSource: 'm: arg
		| a |
		a := instVar + 1.
		^ a'
withNewSelector: #extractedMethod.

	transformation generateChanges.

	self 
		assert: (class parseTreeForSelector: #extractedMethod)
		equals: (self parseMethod: 'extractedMethod 
					| a |
					a := instVar + 1.
					^ a').
		
	self 
		assert: (class parseTreeForSelector: #m:)
		equals: (self parseMethod: 'm: arg
				| a |
				a := self extractedMethod.
				^ a')
]

{ #category : 'tests' }
ReSemanticsOfExtractMethodTransformationTest >> testArgumentUsedInExtractedCodeExpectGotExtracted [

	| transformation |
	transformation := self 
extractSource: 'a := arg + 1' 
fromSource: 'm: arg
		| a |
		a := arg + 1.
		^ a'
withNewSelector: #extractedMethod:.

	transformation generateChanges.

	self 
		assert: (class parseTreeForSelector: #extractedMethod:)
		equals: (self parseMethod: 'extractedMethod: arg 
					| a |
					a := arg + 1.
					^ a').
		
	self 
		assert: (class parseTreeForSelector: #m:)
		equals: (self parseMethod: 'm: arg
				| a |
				a := self extractedMethod: arg.
				^ a')
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
ReSemanticsOfExtractMethodTransformationTest >> testBeginningExpressionsOfASequenceGotExtracted2 [

	| transformation |
	transformation := self 
extractSource: 'self bar.
		self end.' 
fromSource: 'm 
		self foo.
		self bar.
		self end.
		self zoo'
withNewSelector: #extractedMethod.
	transformation generateChanges.
	
self 
		assert: (class parseTreeForSelector: #extractedMethod)
		equals: (self parseMethod: ' extractedMethod

	self bar.
	^ self end').
		
	self 
		assert: (class parseTreeForSelector: #m) 
		equals: (self parseMethod: 'm

	self foo.
	self extractedMethod.
	self zoo')
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
ReSemanticsOfExtractMethodTransformationTest >> testExtractClass [

	| transformation |
	transformation := self 
extractSource: 'Point' 
fromSource: 'm: arg
		| a |
		a := Point new.
		^ a'
withNewSelector: #extractedMethod.

	transformation generateChanges.

	self 
		assert: (class parseTreeForSelector: #extractedMethod)
		equals: (self parseMethod: 'extractedMethod 
					
					^ Point').
		
	self 
		assert: (class parseTreeForSelector: #m:)
		equals: (self parseMethod: 'm: arg
				| a |
				a := self extractedMethod new.
				^ a')
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
ReSemanticsOfExtractMethodTransformationTest >> testExtractClassFromMessageExpectGotExtracted [

	| transformation |
	transformation := self 
extractSource: 'RBParser' 
fromSource: 'm
		instVar := RBParser parseMethod: foo'
withNewSelector: #extractedMethod.

	transformation generateChanges.

	self 
		assert: (class parseTreeForSelector: #extractedMethod)
		equals: (self parseMethod: 'extractedMethod 
					^ RBParser').
		
	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm
				instVar := self extractedMethod parseMethod: foo')
]

{ #category : 'tests' }
ReSemanticsOfExtractMethodTransformationTest >> testExtractLiteralExpectGotExtracted [

	| transformation |
	transformation := self 
extractSource: '42' 
fromSource: 'm
		instVar := 42'
withNewSelector: #extractedMethod.

	transformation generateChanges.

	self 
		assert: (class parseTreeForSelector: #extractedMethod)
		equals: (self parseMethod: 'extractedMethod 
					^ 42').
		
	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm
				instVar := self extractedMethod')
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
ReSemanticsOfExtractMethodTransformationTest >> testExtractSuperExpectExtracted [

	| transformation |
	transformation := self 
extractSource: 'super' 
fromSource: 'm 
		^ super someMethod'
withNewSelector: #extractedMethod.
	transformation generateChanges.
	
self 
		assert: (class parseTreeForSelector: #extractedMethod)
		equals: (self parseMethod: 'extractedMethod 
					^ super').
		
	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm 
				^ self extractedMethod someMethod')
]

{ #category : 'tests - failures' }
ReSemanticsOfExtractMethodTransformationTest >> testExtractedCodeHasMoreThanOneExitPointExpectFailure [

	| transformation |
	transformation := self 
extractSource: 'arg ifOdd: [ ^ false ]. arg ifEven: [ ^ true ].' 
fromSource: 'm: arg
		arg ifOdd: [ ^ false ].
		arg ifEven: [ ^ true ].
		self calculateOn: arg'
withNewSelector: #extractedMethod:.

	self should: [ transformation generateChanges ]
		raise: RBRefactoringWarning
]

{ #category : 'tests - failures' }
ReSemanticsOfExtractMethodTransformationTest >> testExtractedCodeHasTwoExitPointExpectFailure [

	| transformation |
	transformation := self 
extractSource: 'arg ifOdd: [ ^ false ]' 
fromSource: 'm: arg
		arg ifOdd: [ ^ false ].
		self calculateOn: arg'
withNewSelector: #extractedMethod:.

	self should: [ transformation generateChanges ]
		raise: RBRefactoringWarning
]

{ #category : 'tests' }
ReSemanticsOfExtractMethodTransformationTest >> testInstanceVariableIsAssignedInExtractedCodeExpectGotExtracted [

	| transformation |
	transformation := self 
extractSource: 'instVar := instVar + 1' 
fromSource: 'm
		instVar := instVar + 1'
withNewSelector: #extractedMethod.

	transformation generateChanges.

	self 
		assert: (class parseTreeForSelector: #extractedMethod)
		equals: (self parseMethod: 'extractedMethod 
					^ instVar := instVar + 1').
		
	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm
				self extractedMethod')
]

{ #category : 'tests' }
ReSemanticsOfExtractMethodTransformationTest >> testInstanceVariableIsUsedInExtractedCodeExpectGotExtracted [

	| transformation |
	transformation := self 
extractSource: 'a := instVar + 1' 
fromSource: 'm
		| a |
		a := instVar + 1.
		^ a'
withNewSelector: #extractedMethod.

	transformation generateChanges.

	self 
		assert: (class parseTreeForSelector: #extractedMethod)
		equals: (self parseMethod: 'extractedMethod 
					| a |
					a := instVar + 1.
					^ a').
		
	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm
				| a |
				a := self extractedMethod.
				^ a')
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
ReSemanticsOfExtractMethodTransformationTest >> testMultipleInstanceVariableAssignmentsInExtractedCodeExpectGotExtracted [

	| transformation |
	transformation := self 
extractSource: 'instVar := instVar + 1. instVar2 := instVar2 + instVar' 
fromSource: 'm
		instVar := instVar + 1.
		instVar2 := instVar2 + instVar'
withNewSelector: #extractedMethod.

	transformation generateChanges.

	self 
		assert: (class parseTreeForSelector: #extractedMethod)
		equals: (self parseMethod: 'extractedMethod 
					instVar := instVar + 1.
					^ instVar2 := instVar2 + instVar').
		
	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm
				self extractedMethod')
]

{ #category : 'tests' }
ReSemanticsOfExtractMethodTransformationTest >> testMultipleTempAssignmentWithOneReferencedAfterExtractedCodeExpectGotExtracted [

	| transformation |
	transformation := self 
extractSource: 'a := 2. b := self foo: 8.' 
fromSource: 'm 
		| a b |
		a := 2.
		b := self foo: 8.
		^ a'
withNewSelector: #extractedMethod.

	transformation generateChanges.

	self 
		assert: (class parseTreeForSelector: #extractedMethod)
		equals: (self parseMethod: 'extractedMethod 
					| a b |
					a := 2. 
					b := self foo: 8.
					^ a').
		
	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm
				| a |
				a := self extractedMethod.
				^ a')
]

{ #category : 'tests - failures' }
ReSemanticsOfExtractMethodTransformationTest >> testMultipleTempAssignmentWithReferencesAfterExtractedCodeExpectFailure [

	| transformation |
	transformation := self 
extractSource: 'a := 2. b := self foo: 8.' 
fromSource: 'm 
		| a b |
		a := 2.
		b := self foo: 8.
		^ a + b'
withNewSelector: #extractedMethod.

	self should: [ transformation generateChanges ]
		raise: RBRefactoringError
]

{ #category : 'tests' }
ReSemanticsOfExtractMethodTransformationTest >> testMultipleTempAssignmentWithoutReferencesAfterExtractedCodeExpectGotExtracted [

	| transformation |
	transformation := self 
extractSource: 'a := 2. b := self foo: 8.' 
fromSource: 'm 
		| a b |
		a := 2.
		b := self foo: 8.
		self end'
withNewSelector: #extractedMethod.
	transformation generateChanges.
	
self 
		assert: (class parseTreeForSelector: #extractedMethod)
		equals: (self parseMethod: 'extractedMethod 
					| a b |
					a := 2. 
					^ b := self foo: 8').
		
	self 
		assert: (class parseTreeForSelector: #m)
		equals: (self parseMethod: 'm
				self extractedMethod.
				self end')
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
