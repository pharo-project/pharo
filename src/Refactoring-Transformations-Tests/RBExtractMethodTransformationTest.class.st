Class {
	#name : 'RBExtractMethodTransformationTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'private' }
RBExtractMethodTransformationTest >> sourceCodeAt: anInterval forMethod: aSelector in: aClass [

	^ (aClass sourceCodeAt: aSelector)
		copyFrom: anInterval first to: anInterval last
]

{ #category : 'tests' }
RBExtractMethodTransformationTest >> testExtractUsingExistingMethodRefactoring [

	| transformation class |
	model := self modelOnClasses: { RBTransformationRuleTestData }.
	transformation := RBExtractMethodTransformation
		                  model: model
		                  extract: 'rewriteRule tree printString'
		                  from: #checkMethod:
		                  to: #bar
		                  in: #RBTransformationRuleTestData.
	transformation generateChanges.
	self assert: transformation model changes changes size equals: 1.

	class := transformation model classNamed:
		         #RBTransformationRuleTestData.
	self
		assert: (class parseTreeForSelector: #checkMethod:)
		equals: (self parseMethod: 'checkMethod: aSmalllintContext
	class := aSmalllintContext selectedClass.
	(rewriteRule executeTree: aSmalllintContext parseTree) ifTrue: [
		(RecursiveSelfRule
			 executeTree: rewriteRule tree
			 initialAnswer: false) ifFalse: [
			builder
				compile: self rewriteRuleAsString
				in: class
				classified: aSmalllintContext protocols ] ]').
	self deny: (class directlyDefinesMethod: #bar)
]

{ #category : 'tests - failures' }
RBExtractMethodTransformationTest >> testFailureBadInterval [

	self shouldFail: (RBExtractMethodTransformation
							extract: 'whatever'
							from: #testMethod to: #bla
							in: #RBClassDataForRefactoringTest).

	self shouldFail: (RBExtractMethodTransformation
							extract: (self sourceCodeAt: (80 to: 147)
								forMethod: #subclassOf:overrides: in: RBBasicLintRuleTestData class)
							from: #subclassOf:overrides: to: #bla
							in: #'RBBasicLintRuleTestData class')
]

{ #category : 'tests - failures' }
RBExtractMethodTransformationTest >> testFailureCannotExtractProperSubtreeFromInterval [

	self shouldFail: (RBExtractMethodTransformation
			 extract: (self
					  sourceCodeAt: (80 to: 269)
					  forMethod: #subclassOf:overrides:
					  in: RBBasicLintRuleTestData class)
			 from: #subclassOf:overrides:
			 to: #foo
			 in: #'RBBasicLintRuleTestData class').

	self shouldFail: (RBExtractMethodTransformation
			 extract: (self
					  sourceCodeAt: (53 to: 56)
					  forMethod: #subclassOf:overrides:
					  in: RBBasicLintRuleTestData class)
			 from: #subclassOf:overrides:
			 to: #foo
			 in: #'RBBasicLintRuleTestData class').
]

{ #category : 'tests' }
RBExtractMethodTransformationTest >> testFailureCannotExtractTwoAssignmentsToTemporaries [

	self shouldFail: (RBExtractMethodTransformation
			 extract: (self
					  sourceCodeAt: (77 to: 222)
					  forMethod: #subclassResponsibilityNotDefined
					  in: RBBasicLintRuleTestData class)
			 from: #subclassResponsibilityNotDefined
			 to: #foo
			 in: #'RBBasicLintRuleTestData class')
]

{ #category : 'tests - failures' }
RBExtractMethodTransformationTest >> testFailureWhenMethodDoesNotExist [

	self shouldFail: (RBExtractMethodTransformation
			extract: 'bla'
			from: #checkClass1:
			to: #bla
			in: #RBBasicLintRuleTestData)
			
]

{ #category : 'tests' }
RBExtractMethodTransformationTest >> testFailureWhenTemporaryReadBeforeWritten [

	| class method |
	model := self modelOnClasses: { self class }.
	class := model classNamed: self class name.
	method := 'foo
			| temp bar |
			bar := temp.
			temp := bar * bar.
			Transcript show: temp printString; cr.
			^temp * temp'.
	class compile: method classified: #( #accessing ).

	self
		should: [
			(RBExtractMethodTransformation
				 model: model
				 extract: '
			bar := temp.
			temp := bar * bar.'
				 from: #foo
				 to: #foobar
				 in: class name) generateChanges ]
		raise: RBRefactoringError
]

{ #category : 'tests' }
RBExtractMethodTransformationTest >> testNeedsReturn [
	"When code to be extracted contains explicit returns,
	then when replacing it with the extracted method name,
	we should return the result of the method"

	| refactoring class |
	model := self modelOnClasses: { RBDummyLintRuleTest }.
	refactoring := [(RBExtractMethodTransformation
		                model: model
		                extract: 'rules isEmpty ifTrue: [^self].
						rules size == 1 ifTrue: [^rules first viewResults]'
		                from: #openEditor
		                to: #foo:
		                in: #RBDummyLintRuleTest) generateChanges ]
		               on: RBRefactoringWarning
		               do: [ :ex | ex resume ].

	self assert: refactoring model changes changes size equals: 2.

	class := refactoring model classNamed: #RBDummyLintRuleTest.
	self
		assert: (class parseTreeForSelector: #openEditor)
		equals: (self parseMethod: 'openEditor
				| rules |
				rules := self failedRules.
				^self foo: rules').
	self
		assert: (class parseTreeForSelector: #foo:)
		equals: (self parseMethod: 'foo: rules
				rules isEmpty ifTrue: [^self].
				^ rules size == 1 ifTrue: [^rules first viewResults]')
]

{ #category : 'tests - utilities' }
RBExtractMethodTransformationTest >> testSelectorStartingFromArgumentsSize [

	self 
		assert: (RBExtractMethodTransformation new selectorStartingFrom: 'foo' argumentsSize: 2)
		equals: #'foo_:_:'.
	self 
		assert: (RBExtractMethodTransformation new selectorStartingFrom: 'foo' argumentsSize: 0)
		equals: #foo.
	self 
		assert: (RBExtractMethodTransformation new selectorStartingFrom: '' argumentsSize: 2)
		equals: #_:_:.
]

{ #category : 'tests' }
RBExtractMethodTransformationTest >> testTransform [

	| transformation class |
	model := self modelOnClasses: { self changeMockClass }.

	transformation := (RBAddMethodTransformation
		                   model: model
		                   sourceCode: 'foo
									| temp bar |
									bar := 5.
									temp := bar * bar.
									Transcript show: temp printString; cr.
									^temp * temp'
		                   in: self changeMockClass name
		                   withProtocol: #accessing) generateChanges.

	transformation := (RBExtractMethodTransformation
		                   model: transformation model
		                   extract: 'bar := 5.
							temp := bar * bar.
							Transcript show: temp printString; cr'
		                   from: #foo
		                   to: #extractedMethod
		                   in: self changeMockClass name) generateChanges.

	self assert: transformation model changes changes size equals: 4.

	class := transformation model classNamed: self changeMockClass name.
	self
		assert: (class parseTreeForSelector: #foo)
		equals: (self parseMethod: 'foo
													| temp |
													temp := self extractedMethod.
													^temp * temp').
	self
		assert: (class parseTreeForSelector: #extractedMethod)
		equals: (self parseMethod: 'extractedMethod
													| temp bar |
													bar := 5.
													temp := bar * bar.
													Transcript show: temp printString; cr.
													^temp.')
]

{ #category : 'tests' }
RBExtractMethodTransformationTest >> testWhenTemporaryVariableBecomesArgumentOfExtractedMethod [

	| refactoring class |
	model := self modelOnClasses: { RBDummyLintRuleTest }.
	refactoring := (RBExtractMethodTransformation
							 model: model
		                extract: (self
				                 sourceCodeAt: (78 to: 197)
				                 forMethod: #displayName
				                 in: RBDummyLintRuleTest)
		                from: #displayName
		                to: #foo:
		                in: #RBDummyLintRuleTest) generateChanges.

	self assert: refactoring model changes changes size equals: 2.

	class := refactoring model classNamed: #RBDummyLintRuleTest.
	self
		assert: (class parseTreeForSelector: #displayName)
		equals: (self parseMethod: 'displayName
					| nameStream |
					nameStream := WriteStream on: (String new: 64).
					self foo: nameStream.
					^nameStream contents').
	self
		assert: (class parseTreeForSelector: #foo:)
		equals: (self parseMethod: 'foo: nameStream
					nameStream nextPutAll: self name;
								nextPutAll: '' (''.
					self problemCount printOn: nameStream.
					^ nameStream nextPut: $).')
]

{ #category : 'tests' }
RBExtractMethodTransformationTest >> testWithArgument [

	| refactoring class |
	model := self modelOnClasses: { RBTransformationRuleTestData }.

	refactoring := (RBExtractMethodTransformation
		                model: model
		                extract:
			                '(RecursiveSelfRule executeTree: rewriteRule tree initialAnswer: false)
			ifFalse: [builder
						compile: rewriteRule tree printString
						in: class
						classified: aSmalllintContext protocols]'
		                from: #checkMethod:
		                to: #foo:
		                in: #RBTransformationRuleTestData) generateChanges.

	self assert: refactoring model changes changes size equals: 2.

	class := refactoring model classNamed: #RBTransformationRuleTestData.
	self
		assert: (class parseTreeForSelector: #checkMethod:)
		equals: (self parseMethod: 'checkMethod: aSmalllintContext
					class := aSmalllintContext selectedClass.
					(rewriteRule executeTree: aSmalllintContext parseTree) ifTrue:
						[self foo: aSmalllintContext]').
	self
		assert: (class parseTreeForSelector: #foo:)
		equals: (self parseMethod: 'foo: aSmalllintContext
					^ (RecursiveSelfRule executeTree: rewriteRule tree initialAnswer: false)
						ifFalse:
							[builder compile: rewriteRule tree printString
										in: class
										classified: aSmalllintContext protocols]')
]

{ #category : 'tests' }
RBExtractMethodTransformationTest >> testWithTemporariesSelected [

	| class refactoring |
	model := self modelOnClasses: { self class }.
	class := model classNamed: self class name.
	class
		compile: 'foo [| temp | temp := 5. temp * temp] value'
		classified: #( #accessing ).

	refactoring := (RBExtractMethodTransformation
		                model: model
		                extract: '| temp | temp := 5. temp * temp'
		                from: #foo
		                to: #foobar
		                in: class name) generateChanges.

	self assert: refactoring model changes changes size equals: 4.
	self
		assert: (class parseTreeForSelector: #foo)
		equals: (self parseMethod: 'foo [self foobar] value').
	self
		assert: (class parseTreeForSelector: #foobar)
		equals: (self parseMethod: 'foobar |temp | temp := 5. ^temp * temp')
]

{ #category : 'tests' }
RBExtractMethodTransformationTest >> testWithTemporaryAssigned [
	"test when selected code contains temporary that has assignment
	extracted method will also have that temporary"

	| class method refactoring |
	model := self modelOnClasses: { self class }.
	class := model classNamed: self class name.
	method := 'foo
			| temp bar |
			bar := 5.
			temp := bar * bar.
			Transcript show: temp printString; cr.
			^temp * temp'.
	class compile: method classified: #( #accessing ).

	refactoring := (RBExtractMethodTransformation
		                model: model
		                extract: 'bar := 5.
			temp := bar * bar.
			Transcript show: temp printString; cr.'
		                from: #foo
		                to: #foobar
		                in: class name) generateChanges.

	self assert: refactoring model changes changes size equals: 4.
	self
		assert: (class parseTreeForSelector: #foo)
		equals:
		(self parseMethod: 'foo | temp | temp := self foobar. ^temp * temp').
	self
		assert: (class parseTreeForSelector: #foobar)
		equals: (self parseMethod:
				 'foobar | temp bar | bar := 5. temp := bar * bar. Transcript show: temp printString; cr. ^temp.')
]

{ #category : 'tests' }
RBExtractMethodTransformationTest >> testWithTemporaryVariable [

	| refactoring class |
	model := self modelOnClasses: { RBTransformationRuleTestData }.
	refactoring := (RBExtractMethodTransformation
		                model: model
		                extract: (self
				                 sourceCodeAt: (22 to: 280)
				                 forMethod: #superSends
				                 in: RBTransformationRuleTestData)
		                from: #superSends
		                to: #foo1
		                in: #RBTransformationRuleTestData) generateChanges.

	self assert: refactoring model changes changes size equals: 2.

	class := refactoring model classNamed: #RBTransformationRuleTestData.
	self
		assert: (class parseTreeForSelector: #superSends)
		equals: (self parseMethod: 'superSends
				| rule |
				rule := self foo1.
				self rewriteUsing: rule').
	self
		assert: (class parseTreeForSelector: #foo1)
		equals: (self parseMethod: 'foo1 | rule |
				rule := RBParseTreeRewriter new.
				rule addSearch: ''super `@message: ``@args''
					-> ([:aNode |
					(class withAllSubclasses
						detect: [:each | each includesSelector: aNode selector]
						ifNone: [nil]) isNil]
							-> ''self `@message: ``@args'').
				^rule')
]
