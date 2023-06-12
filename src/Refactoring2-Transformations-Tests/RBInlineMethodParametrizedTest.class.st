Class {
	#name : #RBInlineMethodParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBInlineMethodParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBInlineMethodRefactoring };
		addCase: { #rbClass -> RBInlineMethodTransformation };
		yourself
]

{ #category : #accessing }
RBInlineMethodParametrizedTest >> constructor [
	^ #inline:inMethod:forClass:
]

{ #category : #'failure tests' }
RBInlineMethodParametrizedTest >> testFailureBadInterval [
	self shouldFail:
		(self createRefactoringWithArguments: { (13 to: 23) . #testMethod . RBClassDataForRefactoringTest }).
	self shouldFail:
		(self createRefactoringWithArguments: { (14 to: 17) . #testMethod . RBClassDataForRefactoringTest }).
	self shouldFail:
		(self createRefactoringWithArguments: { (24 to: 30) . #testMethod . RBClassDataForRefactoringTest }).
	self shouldFail:
		(self createRefactoringWithArguments: { (1 to: 30) . #testMethod . RBClassDataForRefactoringTest })
]

{ #category : #'failure tests' }
RBInlineMethodParametrizedTest >> testFailureInlineMethodForSuperSendThatAlsoSendsSuper [

	| refactoring |
	model := self rbModelForInliningMethodTest.
	refactoring := self createRefactoringWithArguments: {
			               (102 to: 131).
			               #executeNotifying:.
			               #RBRenameInstanceVariableChange }.
	self shouldFail: refactoring
]

{ #category : #'failure tests' }
RBInlineMethodParametrizedTest >> testFailureNonExistantSelector [
	self shouldFail: (self createRefactoringWithArguments:
		{ (14 to: 17) . #checkClass1: . RBClassDataForRefactoringTest })
]

{ #category : #'failure tests' }
RBInlineMethodParametrizedTest >> testFailureOverriden [

	self shouldWarn: (self createRefactoringWithArguments: {
				 (15 to: 26).
				 #failedRules.
				 RBLintRuleTestData })
]

{ #category : #'failure tests' }
RBInlineMethodParametrizedTest >> testFailurePrimitive [

	self shouldFail: (self createRefactoringWithArguments: {
				 (14 to: 23).
				 #testMethod.
				 RBClassDataForRefactoringTest })
]

{ #category : #'failure tests' }
RBInlineMethodParametrizedTest >> testFailureReturn [

	self shouldFail: (self createRefactoringWithArguments: {
				 (418 to: 485).
				 #utilityMethods.
				 RBBasicLintRuleTestData class })
]

{ #category : #tests }
RBInlineMethodParametrizedTest >> testInlineMethod [
	| refactoring |
	refactoring := self createRefactoringWithArguments:
		{ (451 to: 500) .#sentNotImplementedInApplication . RBBasicLintRuleTestData class }.

	self executeRefactoring: refactoring.

	self assert: ((refactoring model metaclassNamed: #RBBasicLintRuleTestData) parseTreeForSelector: #sentNotImplementedInApplication) equals: (self parseMethod: 'sentNotImplementedInApplication
									| detector |
									detector := self new.
									detector name: ''Messages sent but not implemented in application''.
									detector methodBlock:
											[:context :result |
											| message class block |
											message := context messages
														detect: [:each | (context isItem: each in: context application) not]
														ifNone: [nil].
											class := context selectedClass.
											block :=
													[:each |
													| app methodApp root |
													app := context application.
													((class canUnderstand: each)
														ifTrue:
															[root := app rootApplication.
															methodApp := ((class whichClassIncludesSelector: each)
																		compiledMethodAt: each) application
																		rootApplication.
															methodApp == root or: [root isBasedOn: methodApp]]
														ifFalse: [false]) not].
											message isNil
												ifTrue: [message := context selfMessages detect: block ifNone: [nil]].
											message isNil
												ifTrue:
													[class := class superclass.
													class isNil
														ifTrue:
															[context superMessages isEmpty
																ifFalse: [message := context superMessages asArray first]]
														ifFalse: [message := context superMessages detect: block ifNone: [nil]]].
											message notNil
												ifTrue:
													[result addSearchString: message.
													result addClass: context selectedClass selector: context selector]].
									^detector')
]

{ #category : #tests }
RBInlineMethodParametrizedTest >> testInlineMethod1 [
	| refactoring |
	refactoring := self createRefactoringWithArguments:
		{ (39 to: 84) . #caller . RBClassDataForRefactoringTest }.
	self setupInlineExpressionFor: refactoring toReturn: false.

	self executeRefactoring: refactoring.

	self assert: ((refactoring model classNamed: #RBClassDataForRefactoringTest) parseTreeForSelector: #caller) equals: (self parseMethod: 'caller
									| anObject anObject1 |
									anObject := 5.
									anObject1 := anObject + 1.
									Transcript
										show: anObject1 printString;
										cr.
									^anObject')
]

{ #category : #tests }
RBInlineMethodParametrizedTest >> testInlineMethod2 [
	| refactoring methodName |
	methodName := #caller1.
	refactoring := self createRefactoringWithArguments:
		{ (40 to: 120) .  methodName . RBClassDataForRefactoringTest }.
	self setupInlineExpressionFor: refactoring toReturn: false.

	self executeRefactoring: refactoring.

	self assert: ((refactoring model classNamed: #RBClassDataForRefactoringTest) parseTreeForSelector: methodName) equals: (self parseMethod: 'caller1
								| anObject each1 anObject1 |
								anObject := 5.
								anObject1 := anObject + 1.
								each1 := anObject1 printString.
								Transcript
									show: each1;
									cr.
								[:each | each printString. ^anObject] value: each1')
]

{ #category : #tests }
RBInlineMethodParametrizedTest >> testInlineMethod3 [
	| refactoring methodName |
	methodName := #caller2.
	refactoring := self createRefactoringWithArguments:
		{ (58 to: 73) . methodName . RBClassDataForRefactoringTest }.
	self setupInlineExpressionFor: refactoring toReturn: false.

	self executeRefactoring: refactoring.

	self assert: ((refactoring model classNamed: #RBClassDataForRefactoringTest) parseTreeForSelector: methodName) equals: (self parseMethod: 'caller2
								^(1 to: 10) inject: 1 into: [:sum :each | sum * ((1 to: 10) inject: each into: [:sum1 :each1 | sum1 + each1])]	')
]

{ #category : #tests }
RBInlineMethodParametrizedTest >> testInlineMethod4 [
	| refactoring |
	refactoring := self createRefactoringWithArguments:
		{ (31 to: 112) . #inlineJunk . RBClassDataForRefactoringTest }.
	self setupInlineExpressionFor: refactoring toReturn: false.

	self executeRefactoring: refactoring.

	self assert: ((refactoring model classNamed: #RBClassDataForRefactoringTest) parseTreeForSelector: #inlineJunk) equals: (self parseMethod: 'inlineJunk
										| asdf bar1 baz1 asdf1 |
										bar1 :=
												[:each |
												| temp |
												temp := each.
												temp , temp] value: self.
										baz1 := bar1 + bar1.
										asdf1 := baz1 + bar1.
										asdf := asdf1.
										^asdf foo:
												[:bar |
												| baz |
												baz := bar.
												baz * baz]')
]

{ #category : #tests }
RBInlineMethodParametrizedTest >> testInlineMethod5 [
	| refactoring |

	refactoring := self createRefactoringWithArguments:
		{ (53 to: 64) . #inlineLast . RBClassDataForRefactoringTest }.
	self executeRefactoring: refactoring.

	self assert: ((refactoring model classNamed: #RBClassDataForRefactoringTest) parseTreeForSelector: #inlineLast) equals: (self parseMethod: 'inlineLast
									5 = 3 ifTrue: [^self caller] ifFalse: [^	(1 to: 10) inject: 1 into: [:sum :each | sum * (self foo: each)]]')
]

{ #category : #tests }
RBInlineMethodParametrizedTest >> testInlineMethodForSuperSend [
	| refactoring |
	model := self rbModelForInliningMethodTest.
	(model classNamed: #RBRenameVariableChange)
		removeMethod: #executeNotifying:.
	refactoring := self createRefactoringWithModel: model andArguments:
		{ (101 to: 130) . #executeNotifying: . (model classNamed: #RBRenameInstanceVariableChange) }.
	self executeRefactoring: refactoring.

	self
		assert: ((model classNamed: #RBRenameInstanceVariableChange)
				parseTreeForSelector: #executeNotifying:) equals: (self
							parseMethod: 'executeNotifying: aBlock
									| undo undos undo1 |
									self addNewVariable.
									self copyOldValuesToNewVariable.
									undos := changes collect: [:each | each executeNotifying: aBlock].
									undo1 := self copy.
									undo1 changes: undos reverse.
									undo := undo1.
									self removeOldVariable.
									^undo')
]

{ #category : #tests }
RBInlineMethodParametrizedTest >> testInlineRecursiveCascadedMethod [
	| refactoring |
	refactoring := self createRefactoringWithArguments:
		{ (33 to: 62) . #inlineMethod . RBClassDataForRefactoringTest }.

	self executeRefactoring: refactoring.

	self assert: ((refactoring model classNamed: #RBClassDataForRefactoringTest) parseTreeForSelector: #inlineMethod) equals: (self parseMethod: 'inlineMethod
									| temp temp1 |
									self foo.
									temp1 := self foo; inlineMethod; bar.
									temp := self bar.
									^temp')
]

{ #category : #tests }
RBInlineMethodParametrizedTest >> testModelInlineRecursiveMethod [
	| refactoring class |
	class := model classNamed: #Object.
	class compile: 'foo self bar. self foo. self bar' classified: #(#accessing).
	refactoring := self createRefactoringWithModel: model andArguments:
		{ (15 to: 23) . #foo . class }.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	self assert: (class parseTreeForSelector: #foo)
		equals: (self parseMethod: 'foo self bar. self bar. self foo. self bar. self bar')
]
