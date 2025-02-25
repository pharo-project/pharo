Class {
	#name : 'RBExtractMethodRefactoringTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'tests' }
RBExtractMethodRefactoringTest >> testExtractMethodAtEndOfMethodThatNeedsReturn [
	| refactoring class |
	refactoring := RBExtractMethodRefactoring extract: (52 to: 133) from: #openEditor in: RBLintRuleTestData.
	self setupMethodNameFor: refactoring toReturn: #foo:.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBLintRuleTestData.

	self assert: (class parseTreeForSelector: #openEditor) equals: (self parseMethod: 'openEditor
	| rules |
	rules := self failedRules.
	^self foo: rules').
	self assert: (class parseTreeForSelector: #foo:) equals: (self parseMethod: 'foo: rules
	rules isEmpty ifTrue: [^self].
	rules size == 1 ifTrue: [^rules first viewResults]')
]

{ #category : 'tests' }
RBExtractMethodRefactoringTest >> testExtractMethodThatMovesTemporaryVariable [
	| refactoring class |
	refactoring := RBExtractMethodRefactoring extract: (22 to: 280) from: #superSends in: RBTransformationRuleTestData.
	self setupMethodNameFor: refactoring toReturn: #foo1.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBTransformationRuleTestData.

	self assert: (class parseTreeForSelector: #superSends) equals: (self parseMethod: 'superSends
	| rule |
	rule := self foo1.
	self rewriteUsing: rule').
	self assert: (class parseTreeForSelector: #foo1) equals: (self parseMethod: 'foo1 | rule | 	rule := OCParseTreeRewriter new.
	rule addSearch: ''super `@message: ``@args''
				-> (
					[:aNode |
					(class withAllSubclasses
						detect: [:each | each includesSelector: aNode selector]
						ifNone: [nil]) isNil]
							-> ''self `@message: ``@args'').
		^rule')
]

{ #category : 'tests' }
RBExtractMethodRefactoringTest >> testExtractMethodThatNeedsArgument [
	| refactoring class |
	refactoring := RBExtractMethodRefactoring extract: (143 to: 340) from: #checkMethod: in: RBTransformationRuleTestData.
	self setupMethodNameFor: refactoring toReturn: #foo:.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBTransformationRuleTestData.

	self assert: (class parseTreeForSelector: #checkMethod:) equals: (self parseMethod: 'checkMethod: aSmalllintContext
	class := aSmalllintContext selectedClass.
	(rewriteRule executeTree: aSmalllintContext parseTree) ifTrue:
			[self foo: aSmalllintContext]').
	self assert: (class parseTreeForSelector: #foo:) equals: (self parseMethod: 'foo: aSmalllintContext (RecursiveSelfRule executeTree: rewriteRule tree initialAnswer: false)
				ifFalse:
					[builder compile: rewriteRule tree printString
						in: class
						classified: aSmalllintContext protocols]')
]

{ #category : 'tests' }
RBExtractMethodRefactoringTest >> testExtractMethodThatNeedsTemporaryVariable [
	| refactoring class |
	refactoring := RBExtractMethodRefactoring extract: (78 to: 197) from: #displayName in: RBLintRuleTestData.
	self setupMethodNameFor: refactoring toReturn: #foo:.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBLintRuleTestData.

	self assert: (class parseTreeForSelector: #displayName) equals: (self parseMethod: 'displayName
	| nameStream |
	nameStream := WriteStream on: (String new: 64).
	self foo: nameStream.
	^nameStream contents').

	self assert: (class parseTreeForSelector: #foo:) equals: (self parseMethod: 'foo: nameStream 	nameStream nextPutAll: self name;
		nextPutAll: '' (''.
	self problemCount printOn: nameStream.
	nameStream nextPut: $).')
]

{ #category : 'tests' }
RBExtractMethodRefactoringTest >> testExtractMethodToSuperclass [
	| refactoring class |
	refactoring := RBExtractMethodRefactoring extractSource: '(RecursiveSelfRule executeTree: rewriteRule tree initialAnswer: false) ifFalse:
		[builder compile: rewriteRule tree printString
		in: class
		classified: aSmalllintContext protocols]' from: #checkMethod: in: RBTransformationRuleTestData.
	self setupMethodNameFor: refactoring toReturn: #foo:.
	self setupExtractionClassFor: refactoring toReturn: RBFooLintRuleTestData.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBTransformationRuleTestData.

	self assert: (class parseTreeForSelector: #checkMethod:) equals: (self parseMethod: 'checkMethod: aSmalllintContext
	class := aSmalllintContext selectedClass.
	(rewriteRule executeTree: aSmalllintContext parseTree) ifTrue:
			[self foo: aSmalllintContext]').

	class := refactoring model classNamed: #RBFooLintRuleTestData.
	self assert: (class parseTreeForSelector: #foo:) equals: (self parseMethod: 'foo: aSmalllintContext (RecursiveSelfRule executeTree: rewriteRule tree initialAnswer: false)
				ifFalse:
					[builder compile: rewriteRule tree printString
						in: class
						classified: aSmalllintContext protocols]')
]

{ #category : 'tests' }
RBExtractMethodRefactoringTest >> testExtractWithRenamingOfParameters [
	| refactoring class |
	refactoring := RBExtractMethodRefactoring extract: (78 to: 197) from: #displayName in: RBLintRuleTestData.
	self setupMethodNameFor: refactoring toReturn: #foo:.
	refactoring
		parameterMap:
			(Array with: ((RBArgumentName name: #nameStream) newName: #newParameter)).
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBLintRuleTestData.

	self
		assert: (class parseTreeForSelector: #displayName)
		equals:
			(self
				parseMethod:
					'displayName
	| nameStream |
	nameStream := WriteStream on: (String new: 64).
	self foo: nameStream.
	^nameStream contents').
	"Extracted method with renamed parameters"
	self
		assert: (class parseTreeForSelector: #foo:)
		equals:
			(self
				parseMethod:
					'foo: newParameter 	newParameter nextPutAll: self name;
		nextPutAll: '' (''.
	self problemCount printOn: newParameter.
	newParameter nextPut: $).')
]

{ #category : 'failure tests' }
RBExtractMethodRefactoringTest >> testFailureBadInterval [
	self shouldFail: (RBExtractMethodRefactoring extract: (24 to: 30) from: #testMethod in: RBClassDataForRefactoringTest ).
	self shouldFail: (RBExtractMethodRefactoring extract: (80 to: 147) from: #subclassOf:overrides: in: RBBasicLintRuleTestData class )
]

{ #category : 'failure tests' }
RBExtractMethodRefactoringTest >> testFailureExtract [
	self shouldFail: (RBExtractMethodRefactoring extract: (80 to: 269) from: #subclassOf:overrides: in: RBBasicLintRuleTestData class ).
	self shouldFail: (RBExtractMethodRefactoring extract: (53 to: 56) from: #subclassOf:overrides: in: RBBasicLintRuleTestData class).
	self shouldFail: (RBExtractMethodRefactoring extract: (77 to: 222) from: #subclassResponsibilityNotDefined in: RBBasicLintRuleTestData class )
]

{ #category : 'failure tests' }
RBExtractMethodRefactoringTest >> testFailureExtractMethodWhenGivenSourceIsNotInTheMethod [

	self
		should: [
			RBExtractMethodRefactoring
				extractSource: 'some source that is not from the method'
				from: #subclassOf:overrides:
				in: RBBasicLintRuleTestData class ]
		raise: RBRefactoringError
]

{ #category : 'failure tests' }
RBExtractMethodRefactoringTest >> testFailureNonExistantSelector [
	self shouldFail: (RBExtractMethodRefactoring extract: (10 to: 20) from: #checkClass1: in: RBBasicLintRuleTestData)
]

{ #category : 'tests' }
RBExtractMethodRefactoringTest >> testModelExtractMethodWithTemporariesSelected [
	| class refactoring |
	model := RBNamespace new.
	class := model classNamed: self class name.
	class compile: 'foo [| temp | temp := 5. temp * temp] value'
		classified: #(#accessing).
	refactoring := RBExtractMethodRefactoring
						model: model
						extract: (6 to: 36)
						from: #foo 
						in: class.
	self setupMethodNameFor: refactoring toReturn: #foobar.
	self executeRefactoring: refactoring.
	
	self 
		assert: (class parseTreeForSelector: #foo)
		equals: (self parseMethod: 'foo [self foobar] value').
	
	self
		assert: (class parseTreeForSelector: #foobar)
		equals: (self parseMethod: 'foobar |temp | temp := 5. ^temp * temp')
]

{ #category : 'tests' }
RBExtractMethodRefactoringTest >> testModelExtractMethodWithTemporaryAssigned [
	| class refactoring |
	model := RBNamespace new.
	class := model classNamed: self class name.
	class
		compile: 'foo 				| temp bar | 				bar := 5. 				temp := bar * bar. 				Transcript show: temp printString; cr. 				^temp * temp'
		classified: #(#accessing).
	refactoring := RBExtractMethodRefactoring
						model: model
						extract: (26 to: 102)
						from: #foo
						in:class.
	self setupMethodNameFor: refactoring toReturn: #foobar.
	self executeRefactoring: refactoring.
	self assert: (class parseTreeForSelector: #foo)
			equals: (self parseMethod: 'foo | temp | temp := self foobar. ^temp * temp').
	self
		assert: ((class parseTreeForSelector: #foobar) = (self
						parseMethod: 'foobar | bar temp | bar := 5. temp := bar * bar. Transcript show: temp printString; cr. ^temp.')) |
				((class parseTreeForSelector: #foobar) = (self
						parseMethod: 'foobar | temp bar | bar := 5. temp := bar * bar. Transcript show: temp printString; cr. ^temp.'))
]

{ #category : 'tests' }
RBExtractMethodRefactoringTest >> testValidateRenameParameters [
	| refactoring |
	refactoring := RBExtractMethodRefactoring
						model: model
						extract: (78 to: 197)
						from: #displayName
						in: RBLintRuleTestData.
	self setupMethodNameFor: refactoring toReturn: #foo:.
	refactoring parameterMap: (Dictionary new at: #nameStream put: #nameStream; yourself ).
	"Fail when use instance variables as new parameters"
	self should: [ refactoring validateRenameOf: #nameStream to: #name ] raise: RBRefactoringError.
	self should: [refactoring validateRenameOf: #nameStream to: #foo1] raise: RBRefactoringError.
	"Fail when use class variables as new parameters"
	self should: [refactoring validateRenameOf: #nameStream to: #Foo1] raise: RBRefactoringError.
	"Fail when use temporary variables as new parameters"
]
