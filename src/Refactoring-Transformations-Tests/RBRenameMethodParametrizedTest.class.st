Class {
	#name : 'RBRenameMethodParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-Parametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'tests' }
RBRenameMethodParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
				addCase: { #rbClass -> ReRenameMethodRefactoring };
		addCase: { #rbClass -> RBRenameMethodTransformation };
		yourself
]

{ #category : 'accessing' }
RBRenameMethodParametrizedTest >> constructor [
	^ #renameMethod:in:to:permutation:
]

{ #category : 'failure tests' }
RBRenameMethodParametrizedTest >> testFailureWithNonCorrectNumberOfArgs [
	self shouldFail: (self createRefactoringWithArguments:
		{ #checkClass: . RBBasicLintRuleTestData . #checkClass . (1 to: 1) })
]

{ #category : 'tests' }
RBRenameMethodParametrizedTest >> testRenameMethodFromTrait [
	| refactoring class |
	refactoring := self createRefactoringWithArguments:
		{ ('just', 'ForTest') asSymbol . RBClassToRename classSide . #justForTest1 . (1 to: 0)}.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	
	class := refactoring model classNamed: #'RBClassToRename class'.
	self assert: (class parseTreeForSelector: #justForTest1)
		equals: (self parseMethod: (self methodSignatureStringForUnarySymbol: #justForTest1), '^ 42').
	
	self 
		assert: ((refactoring model classNamed: #RBTDummy) 
		parseTreeForSelector: #methodFromTrait) equals: (self parseMethod: (self methodSignatureStringForUnarySymbol: #methodFromTrait), 
'	RBClassToRename justForTest1').
	
	self deny: (class directlyDefinesMethod: ('just', 'ForTest') asSymbol).
	
	self 
		assert: ((refactoring model classNamed: #RBClassUsingSharedPoolForTestData) 
						methodFor: #methodFromTrait) modelClass name 
		equals: #RBTDummy
]

{ #category : 'tests' }
RBRenameMethodParametrizedTest >> testRenameMethodOnlyInSomePackages [
	| refactoring class |
	model := (RBNamespace onEnvironment: (RBPackageEnvironment packageName: 'Refactoring-DataForTesting')).
	refactoring := self createRefactoringWithModel: model andArguments:
		{ ('check', 'Class:') asSymbol . RBBasicLintRuleTestData . #checkClass1: . (1 to: 1) }.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	
	class := refactoring model classNamed: #RBBasicLintRuleTestData.
	
	self 
		assert: (class parseTreeForSelector: #checkClass1:)
		equals: (self parseMethod: (self methodSignatureStringForSymbol: #checkClass1: withArgumentString: ' aSmalllintContext'),
'	^classBlock value: aSmalllintContext value: result')
]

{ #category : 'tests' }
RBRenameMethodParametrizedTest >> testRenameMethodPermuteArgs [
	| refactoring class |
	"We will permute the arguments of the message while the selector will stay the same 
	 so 
		the new method will see its argument changed
		and its users using 1, 2 should be changed to 2 and 1.
	
	demoRenameMethod: arg1 PermuteArgs: arg2
		self do: arg1.
		self do: arg2.
		^ arg1 > arg2
	
	is transformed into 
	
	demoRenameMethod: arg2 PermuteArgs: arg1
		self do: arg1.
		self do: arg2.
		^ arg1 > arg2
	
	and 
	demoExampleCall
		^ self demoRenameMethod: 1 PermuteArgs: 2
	
	is transformed into 
	
	demoExampleCall
		^ self demoRenameMethod: 2 PermuteArgs: 1
	
	"
	refactoring := self createRefactoringWithArguments:
		{ ('demoRenameMethod:' , 'PermuteArgs:') asSymbol . RBClassDataForRefactoringTest .
		('demoRenameMethod:' , 'PermuteArgs:') asSymbol . #(2 1) }.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBClassDataForRefactoringTest.
	self
		assert:
			(class parseTreeForSelector: ('demoRenameMethod:' , 'PermuteArgs:') asSymbol)
		equals:
			(self
				parseMethod:
'demoRenameMethod: arg2 PermuteArgs: arg1 
	self do: arg1.
	self do: arg2.
	^ arg1 > arg2').
	
	self
		assert:
			(class parseTreeForSelector: #demoExampleCall)
		equals: (self parseMethod: (self methodSignatureStringForUnarySymbol: #demoExampleCall), ' ^self demoRenameMethod: 2 PermuteArgs: 1')
]

{ #category : 'tests' }
RBRenameMethodParametrizedTest >> testRenamePermuteArgs [
	| refactoring class |
	refactoring := self createRefactoringWithArguments:
		{ ('rename:' , 'two:') asSymbol . RBClassDataForRefactoringTest .
		('rename:' , 'two:') asSymbol . #(2 1 ) }.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBClassDataForRefactoringTest.
	
	self 
		assert: (class parseTreeForSelector: ('rename:' , 'two:') asSymbol)
		equals: (self parseMethod: 'rename: argumentMethod two: this ^self printString, this, argumentMethod').
	
	self 
		assert: (class parseTreeForSelector: #exampleCall)
		equals: (self parseMethod: (self methodSignatureStringForUnarySymbol: #exampleCall), '<sampleInstance> ^self rename: 2 two: 1')
]

{ #category : 'tests' }
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

{ #category : 'tests' }
RBRenameMethodParametrizedTest >> testRenameTestMethod [
	| refactoring class |
	refactoring := self createRefactoringWithArguments:
		{ ('rename' , 'ThisMethod:') asSymbol . RBClassDataForRefactoringTest . #renameThisMethod2: . (1 to: 1) }.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBClassDataForRefactoringTest.
	
	self 
		assert: (class parseTreeForSelector: #renameThisMethod2:) 
		equals: (self parseMethod: 'renameThisMethod2: anArg
	^self').
	
	self 
		assert: (class parseTreeForSelector: #callMethod) 
		equals: (self parseMethod: 'callMethod
	^(self renameThisMethod2: 5)').
	self 
		assert: (class parseTreeForSelector: #symbolReference) 
		equals: (self parseMethod: 'symbolReference
		^ #(#renameThisMethod2: #(4 #renameThisMethod2:))').
	self deny: (class directlyDefinesMethod: ('rename' , 'ThisMethod:') asSymbol)
]

{ #category : 'tests' }
RBRenameMethodParametrizedTest >> testRenameTestMethod1 [
	| refactoring class |
	refactoring := self createRefactoringWithArguments:
		{ 'testMethod1' asSymbol . RBClassDataForRefactoringTest . #testMethod2 . (1 to: 0) }.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBClassDataForRefactoringTest.
	
	self 
		assert: (class parseTreeForSelector: #testMethod2)
		equals: (self parseMethod: (self methodSignatureStringForUnarySymbol: #testMethod2), 
'	^self testMethod2 , ([:each | each testMethod2] value: #(#(#testMethod2) 2 #testMethod2))').

	self 
		assert: ((refactoring model classNamed: #RBBasicLintRuleTestData) parseTreeForSelector: #classBlock:) 	
		equals: (self parseMethod: (self methodSignatureStringForSymbol: #classBlock: withArgumentString: ' aBlock'),
'	classBlock := aBlock testMethod2').

	self deny: (class directlyDefinesMethod: ('test' , 'Method1') asSymbol)
]
