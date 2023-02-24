Class {
	#name : #RBRenameMethodParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBRenameMethodParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBRenameMethodRefactoring };
		addCase: { #rbClass -> RBRenameMethodTransformation };
		yourself
]

{ #category : #accessing }
RBRenameMethodParametrizedTest >> constructor [
	^ #renameMethod:in:to:permutation:
]

{ #category : #'failure tests' }
RBRenameMethodParametrizedTest >> testExistingSelector [
	self shouldFail: (self createRefactoringWithArguments:
		{ #checkClass: . RBBasicLintRuleTestData . #runOnEnvironment: . (1 to: 1) })
]

{ #category : #'failure tests' }
RBRenameMethodParametrizedTest >> testMultipleSelectors [
	"Renaming a method with multiple selectors should raise warning"
	self shouldWarn: (self createRefactoringWithArguments:
		{ #checkClass: . RBBasicLintRuleTestData . #foo: . (1 to: 1) })
]

{ #category : #tests }
RBRenameMethodParametrizedTest >> testRenameMethodFromTrait [
	| refactoring class |
	refactoring := self createRefactoringWithArguments:
		{ ('just', 'ForTest') asSymbol . RBClassToRename classSide . #justForTest1 . (1 to: 0)}.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #'RBClassToRename class'.
	self assert: (class parseTreeForSelector: #justForTest1)
		equals: (self parseMethod: 'justForTest1 ^ 42').
	self assert: ((refactoring model classNamed: #RBDummy) parseTreeForSelector: #methodFromTrait) equals: (self parseMethod: 'methodFromTrait
	RBClassToRename justForTest1').
	self deny: (class directlyDefinesMethod: ('just', 'ForTest') asSymbol).
	self assert: ((refactoring model classNamed: #RBClassUsingSharedPoolForTestData) methodFor: #methodFromTrait) modelClass name equals: #RBDummy
]

{ #category : #tests }
RBRenameMethodParametrizedTest >> testRenameMethodOnlyInSomePackages [
	| refactoring class |
	model := (RBClassModelFactory rbNamespace onEnvironment: (RBPackageEnvironment packageName: 'Refactoring-Tests-Core')).
	refactoring := self createRefactoringWithModel: model andArguments:
		{ ('check', 'Class:') asSymbol . RBBasicLintRuleTestData . #checkClass1: . (1 to: 1) }.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBBasicLintRuleTestData.
	self assert: (class parseTreeForSelector: #checkClass1:)
		equals: (self parseMethod: 'checkClass1: aSmalllintContext
	^classBlock value: aSmalllintContext value: result')
]

{ #category : #tests }
RBRenameMethodParametrizedTest >> testRenameMethodPermuteArgs [
	| refactoring class |
	refactoring := self createRefactoringWithArguments:
		{ ('demoRenameMethod:' , 'PermuteArgs:') asSymbol . RBRefactoryTestDataApp .
		('demoRenameMethod:' , 'PermuteArgs:') asSymbol . #(2 1) }.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBRefactoryTestDataApp.
	self
		assert:
			(class parseTreeForSelector: ('demoRenameMethod:' , 'PermuteArgs:') asSymbol)
				equals:
					(self
						parseMethod:
							'demoRenameMethod: arg2 PermuteArgs: arg1 self do: arg1.
	self do: arg2.
	^ arg1 > arg2').
	self
		assert:
			(class parseTreeForSelector: #demoExampleCall)
				equals: (self parseMethod: 'demoExampleCall ^self demoRenameMethod: 2 PermuteArgs: 1')
]

{ #category : #tests }
RBRenameMethodParametrizedTest >> testRenamePermuteArgs [
	| refactoring class |
	refactoring := self createRefactoringWithArguments:
		{ ('rename:' , 'two:') asSymbol . RBRefactoryTestDataApp .
		('rename:' , 'two:') asSymbol . #(2 1 ) }.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBRefactoryTestDataApp.
	self assert: (class parseTreeForSelector: ('rename:' , 'two:') asSymbol)
		equals: (self parseMethod: 'rename: argumentMethod two: this ^self printString, this, argumentMethod').
	self assert: (class parseTreeForSelector: #exampleCall)
		equals: (self parseMethod: 'exampleCall <sampleInstance> ^self rename: 2 two: 1')
]

{ #category : #tests }
RBRenameMethodParametrizedTest >> testRenamePrimitive [
	| refactoring count |
	count := 0.
	model allReferencesTo: #basicAt:put: do: [:method | count := count + 1].
	refactoring := self createRefactoringWithModel: model
		andArguments: { #basicAt:put: . Object . ('at:' , 'bar:') asSymbol . (1 to: 2) }.
	self proceedThroughWarning: [self executeRefactoring: refactoring].
	model allReferencesTo: #basicAt:put:
		do:
			[:method |
			count := count - 1.
			self assert: method source isNil].
	model allReferencesTo: ('at:' , 'bar:') asSymbol
		do: [:method | count := count - 1].
	self assert: count equals: 0
]

{ #category : #tests }
RBRenameMethodParametrizedTest >> testRenameTestMethod [
	| refactoring class |
	refactoring := self createRefactoringWithArguments:
		{ ('rename' , 'ThisMethod:') asSymbol . RBRefactoryTestDataApp . #renameThisMethod2: . (1 to: 1) }.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBRefactoryTestDataApp.
	self assert: (class parseTreeForSelector: #renameThisMethod2:) equals: (self parseMethod: 'renameThisMethod2: anArg
	^self').
	self assert: (class parseTreeForSelector: #callMethod) equals: (self parseMethod: 'callMethod
	^(self renameThisMethod2: 5)').
	self assert: (class parseTreeForSelector: #symbolReference) equals: (self parseMethod: 'symbolReference
		^ #(#renameThisMethod2: #(4 #renameThisMethod2:))').
	self deny: (class directlyDefinesMethod: ('rename' , 'ThisMethod:') asSymbol)
]

{ #category : #tests }
RBRenameMethodParametrizedTest >> testRenameTestMethod1 [
	| refactoring class |
	refactoring := self createRefactoringWithArguments:
		{ ('testMethod1') asSymbol . RBRefactoryTestDataApp . #testMethod2 . (1 to: 0) }.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBRefactoryTestDataApp.
	self assert: (class parseTreeForSelector: #testMethod2)
		equals: (self parseMethod: 'testMethod2
	^self testMethod2 , ([:each | each testMethod2] value: #(#(#testMethod2) 2 #testMethod2))').
	self assert: ((refactoring model classNamed: #RBBasicLintRuleTestData) parseTreeForSelector: #classBlock:) 		equals: (self parseMethod: 'classBlock: aBlock
	classBlock := aBlock testMethod2').
	self deny: (class directlyDefinesMethod: ('test' , 'Method1') asSymbol)
]

{ #category : #'failure tests' }
RBRenameMethodParametrizedTest >> testWithNonCorrectNumberOfArgs [
	self shouldFail: (self createRefactoringWithArguments:
		{ #checkClass: . RBBasicLintRuleTestData . #checkClass . (1 to: 1) })
]
