tests
testInlineMethod
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