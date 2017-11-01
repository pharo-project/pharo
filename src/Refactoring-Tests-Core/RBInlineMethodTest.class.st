Class {
	#name : #RBInlineMethodTest,
	#superclass : #RBRefactoringTest,
	#category : 'Refactoring-Tests-Core'
}

{ #category : #'failure tests' }
RBInlineMethodTest >> testBadInterval [
	self
		shouldFail: (RBInlineMethodRefactoring 
				inline: (self 
						convertInterval: (13 to: 23)
						for: (RBRefactoryTestDataApp sourceCodeAt: #testMethod))
				inMethod: #testMethod
				forClass: RBRefactoryTestDataApp);
		shouldFail: (RBInlineMethodRefactoring 
				inline: (self 
						convertInterval: (14 to: 17)
						for: (RBRefactoryTestDataApp sourceCodeAt: #testMethod))
				inMethod: #testMethod
				forClass: RBRefactoryTestDataApp);
		shouldFail: (RBInlineMethodRefactoring 
				inline: (self 
						convertInterval: (24 to: 30)
						for: (RBRefactoryTestDataApp sourceCodeAt: #testMethod))
				inMethod: #testMethod
				forClass: RBRefactoryTestDataApp);
		shouldFail: (RBInlineMethodRefactoring 
				inline: (self 
						convertInterval: (1 to: 30)
						for: (RBRefactoryTestDataApp sourceCodeAt: #testMethod))
				inMethod: #testMethod
				forClass: RBRefactoryTestDataApp)
]

{ #category : #tests }
RBInlineMethodTest >> testInlineMethod [
	| refactoring |
	refactoring := RBInlineMethodRefactoring 
		inline: (self 
				convertInterval: (455 to: 504)
				for: (RBBasicLintRuleTest class sourceCodeAt: #sentNotImplementedInApplication))
		inMethod: #sentNotImplementedInApplication
		forClass: RBBasicLintRuleTest class.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model metaclassNamed: #RBBasicLintRuleTest) parseTreeFor: #sentNotImplementedInApplication) = (RBParser parseMethod: 'sentNotImplementedInApplication
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
RBInlineMethodTest >> testInlineMethod1 [
	| refactoring |
	refactoring := RBInlineMethodRefactoring 
		inline: (self 
				convertInterval: (39 to: 84)
				for: (RBRefactoryTestDataApp sourceCodeAt: #caller))
		inMethod: #caller
		forClass: RBRefactoryTestDataApp.
	self 
		setupInlineExpressionFor: refactoring
		toReturn: false.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #caller) = (RBParser parseMethod: 'caller 
									| anObject anObject1 | 
									anObject := 5.
									anObject1 := anObject + 1.
									Transcript
										show: anObject1 printString;
										cr.
									^anObject')
]

{ #category : #tests }
RBInlineMethodTest >> testInlineMethod2 [
	| refactoring |
	refactoring := RBInlineMethodRefactoring 
		inline: (self 
				convertInterval: (40 to: 120)
				for: (RBRefactoryTestDataApp sourceCodeAt: #caller1))
		inMethod: #caller1
		forClass: RBRefactoryTestDataApp.
	self 
		setupInlineExpressionFor: refactoring
		toReturn: false.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #caller1) = (RBParser parseMethod: 'caller1 
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
RBInlineMethodTest >> testInlineMethod3 [
	| refactoring |
	refactoring := RBInlineMethodRefactoring 
		inline: (self 
				convertInterval: (58 to: 73)
				for: (RBRefactoryTestDataApp sourceCodeAt: #caller2))
		inMethod: #caller2
		forClass: RBRefactoryTestDataApp.
	self 
		setupInlineExpressionFor: refactoring
		toReturn: false.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #caller2) = (RBParser parseMethod: 'caller2
								^(1 to: 10) inject: 1 into: [:sum :each | sum * ((1 to: 10) inject: each into: [:sum1 :each1 | sum1 + each1])]	')
]

{ #category : #tests }
RBInlineMethodTest >> testInlineMethod4 [
	| refactoring |
	refactoring := RBInlineMethodRefactoring 
		inline: (self 
				convertInterval: (31 to: 112)
				for: (RBRefactoryTestDataApp sourceCodeAt: #inlineJunk))
		inMethod: #inlineJunk
		forClass: RBRefactoryTestDataApp.
	self 
		setupInlineExpressionFor: refactoring
		toReturn: false.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #inlineJunk) = (RBParser parseMethod: 'inlineJunk
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
RBInlineMethodTest >> testInlineMethod5 [
	| refactoring |
	refactoring := RBInlineMethodRefactoring 
		inline: (self 
				convertInterval: (53 to: 64)
				for: (RBRefactoryTestDataApp sourceCodeAt: #inlineLast))
		inMethod: #inlineLast
		forClass: RBRefactoryTestDataApp.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #inlineLast) = (RBParser parseMethod: 'inlineLast
									5 = 3 ifTrue: [^self caller] ifFalse: [^	(1 to: 10) inject: 1 into: [:sum :each | sum * (self foo: each)]]')
]

{ #category : #tests }
RBInlineMethodTest >> testInlineMethodForSuperSend [
	| refactoring |
	model := Smalltalk compiler evaluate: self inlineMethodTestData.
	(model classNamed: #RBRenameVariableChange) removeMethod: #executeNotifying:.
	refactoring := RBInlineMethodRefactoring 
				model: model
				inline: (self convertInterval: (102 to: 131)
						for: ((model classNamed: #RBRenameInstanceVariableChange) 
								sourceCodeFor: #executeNotifying:))
				inMethod: #executeNotifying:
				forClass: (model classNamed: #RBRenameInstanceVariableChange).
	self executeRefactoring: refactoring.
	self 
		assert: ((model classNamed: #RBRenameInstanceVariableChange) 
				parseTreeFor: #executeNotifying:) = (RBParser 
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

{ #category : #'failure tests' }
RBInlineMethodTest >> testInlineMethodForSuperSendThatAlsoSendsSuper [
	| refactoring |
	model := Smalltalk compiler evaluate: self inlineMethodTestData.
	refactoring := RBInlineMethodRefactoring 
				inline: (102 to: 131)
				inMethod: #executeNotifying:
				forClass: (model classNamed: #RBRenameInstanceVariableChange).
	self shouldFail: refactoring
]

{ #category : #tests }
RBInlineMethodTest >> testInlineRecursiveCascadedMethod [
	| refactoring |
	refactoring := RBInlineMethodRefactoring 
		inline: (self 
				convertInterval: (33 to: 62)
				for: (RBRefactoryTestDataApp sourceCodeAt: #inlineMethod))
		inMethod: #inlineMethod
		forClass: RBRefactoryTestDataApp.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #inlineMethod) = (RBParser parseMethod: 'inlineMethod
									| temp temp1 |
									self foo.
									temp1 := self foo; inlineMethod; bar.
									temp := self bar.
									^temp')
]

{ #category : #tests }
RBInlineMethodTest >> testModelInlineRecursiveMethod [
	| refactoring class |
	class := model classNamed: #Object.
	class compile: 'foo self bar. self foo. self bar' classified: #(#accessing).
	refactoring := RBInlineMethodRefactoring 
				model: model
				inline: (15 to: 23)
				inMethod: #foo
				forClass: class.
	self executeRefactoring: refactoring.
	self 
		assert: (class parseTreeFor: #foo) = (RBParser 
						parseMethod: 'foo self bar. self bar. self foo. self bar. self bar')
]

{ #category : #'failure tests' }
RBInlineMethodTest >> testNonExistantSelector [
	self shouldFail: (RBInlineMethodRefactoring 
			inline: (14 to: 17)
			inMethod: #checkClass1:
			forClass: RBRefactoryTestDataApp)
]

{ #category : #'failure tests' }
RBInlineMethodTest >> testOverriden [
	self shouldWarn: (RBInlineMethodRefactoring 
			inline: (self 
					convertInterval: (15 to: 26)
					for: (RBLintRuleTest sourceCodeAt: #failedRules))
			inMethod: #failedRules
			forClass: RBLintRuleTest)
]

{ #category : #'failure tests' }
RBInlineMethodTest >> testPrimitive [
	self shouldFail: (RBInlineMethodRefactoring 
			inline: (self 
					convertInterval: (14 to: 23)
					for: (RBRefactoryTestDataApp sourceCodeAt: #testMethod))
			inMethod: #testMethod
			forClass: RBRefactoryTestDataApp)
]

{ #category : #'failure tests' }
RBInlineMethodTest >> testReturn [
	self shouldFail: (RBInlineMethodRefactoring 
			inline: (self 
					convertInterval: (418 to: 485)
					for: (RBBasicLintRuleTest class sourceCodeAt: #utilityMethods))
			inMethod: #utilityMethods
			forClass: RBBasicLintRuleTest class)
]
