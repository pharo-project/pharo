Class {
	#name : 'RBExtractMethodParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-SingleParametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'SingleParametrized'
}

{ #category : 'tests' }
RBExtractMethodParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBExtractMethodRefactoring };
		yourself
]

{ #category : 'accessing' }
RBExtractMethodParametrizedTest >> constructor [
	^ #extract:from:in:
]

{ #category : 'tests' }
RBExtractMethodParametrizedTest >> testExtractMethodAtEndOfMethodThatNeedsReturn [
	| refactoring class |
	refactoring := self createRefactoringWithArguments: {(52 to: 133) . #openEditor . RBLintRuleTestData }.
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
RBExtractMethodParametrizedTest >> testExtractMethodThatMovesTemporaryVariable [
	| refactoring class |
	refactoring := self createRefactoringWithArguments: { (22 to: 280) . #superSends . RBTransformationRuleTestData }.
	self setupMethodNameFor: refactoring toReturn: #foo1.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBTransformationRuleTestData.

	self assert: (class parseTreeForSelector: #superSends) equals: (self parseMethod: 'superSends
	| rule |
	rule := self foo1.
	self rewriteUsing: rule').
	self assert: (class parseTreeForSelector: #foo1) equals: (self parseMethod: 'foo1 | rule | 	rule := RBParseTreeRewriter new.
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
RBExtractMethodParametrizedTest >> testExtractMethodThatNeedsArgument [
	| refactoring class |
	refactoring := self createRefactoringWithArguments: { (143 to: 340) . #checkMethod: . RBTransformationRuleTestData }.
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
RBExtractMethodParametrizedTest >> testExtractMethodThatNeedsTemporaryVariable [
	| refactoring class |
	refactoring := self createRefactoringWithArguments: { (78 to: 197) . #displayName . RBLintRuleTestData }.
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
RBExtractMethodParametrizedTest >> testExtractMethodToSuperclass [
	| refactoring class |
	refactoring := self createRefactoringWithArguments: { (143 to: 340) . #checkMethod: . RBTransformationRuleTestData }.
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
RBExtractMethodParametrizedTest >> testExtractWithRenamingOfParameters [
	| refactoring class |
	refactoring := self createRefactoringWithArguments: { (78 to: 197) . #displayName . RBLintRuleTestData }.
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
RBExtractMethodParametrizedTest >> testFailureBadInterval [
	self shouldFail: (self createRefactoringWithArguments:
		{ (24 to: 30) . #testMethod . RBClassDataForRefactoringTest }).
	self shouldFail: (self createRefactoringWithArguments:
		{ (80 to: 147) . #subclassOf:overrides: . RBBasicLintRuleTestData class })
]

{ #category : 'failure tests' }
RBExtractMethodParametrizedTest >> testFailureExtract [
	self shouldFail: (self createRefactoringWithArguments:
		{ (80 to: 269) . #subclassOf:overrides: . RBBasicLintRuleTestData class }).
	self shouldFail: (self createRefactoringWithArguments:
		{ (53 to: 56) . #subclassOf:overrides: . RBBasicLintRuleTestData class }).
	self shouldFail: (self createRefactoringWithArguments:
		{ (77 to: 222) . #subclassResponsibilityNotDefined . RBBasicLintRuleTestData class })
]

{ #category : 'failure tests' }
RBExtractMethodParametrizedTest >> testFailureNonExistantSelector [
	self shouldFail: (self createRefactoringWithArguments:
		{ (10 to: 20) . #checkClass1: . RBBasicLintRuleTestData })
]

{ #category : 'tests' }
RBExtractMethodParametrizedTest >> testModelExtractMethodWithTemporariesSelected [
	| class refactoring |
	model := RBNamespace new.
	class := model classNamed: self class name.
	class compile: 'foo [| temp | temp := 5. temp * temp] value'
		classified: #(#accessing).
	refactoring := self createRefactoringWithModel: model
		andArguments: { (6 to: 36) . #foo . class }.
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
RBExtractMethodParametrizedTest >> testModelExtractMethodWithTemporaryAssigned [
	| class refactoring |
	model := RBNamespace new.
	class := model classNamed: self class name.
	class
		compile: 'foo 				| temp bar | 				bar := 5. 				temp := bar * bar. 				Transcript show: temp printString; cr. 				^temp * temp'
		classified: #(#accessing).
	refactoring := self createRefactoringWithModel: model
		andArguments: { (26 to: 102) . #foo . class }.
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
RBExtractMethodParametrizedTest >> testValidateRenameParameters [
	| refactoring |
	refactoring := self createRefactoringWithModel: model
		andArguments: { (78 to: 197) . #displayName . RBLintRuleTestData }.
	self setupMethodNameFor: refactoring toReturn: #foo:.
	refactoring parameterMap: (Dictionary new at: #nameStream put: #nameStream; yourself ).
	"Fail when use instance variables as new parameters"
	self should: [ refactoring validateRenameOf: #nameStream to: #name ] raise: RBRefactoringError.
	self should: [refactoring validateRenameOf: #nameStream to: #foo1] raise: RBRefactoringError.
	"Fail when use class variables as new parameters"
	self should: [refactoring validateRenameOf: #nameStream to: #Foo1] raise: RBRefactoringError.
	"Fail when use temporary variables as new parameters"
]
