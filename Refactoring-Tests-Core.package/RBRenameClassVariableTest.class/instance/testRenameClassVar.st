tests
testRenameClassVar
	| refactoring class |
	refactoring := RBRenameClassVariableRefactoring 
		rename: #RecursiveSelfRule
		to: #RSR
		in: RBTransformationRuleTest.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBTransformationRuleTest.
	self assert: (class directlyDefinesClassVariable: #RSR).
	self deny: (class directlyDefinesClassVariable: #RecursiveSelfRule).
	self assert: (class theMetaClass parseTreeFor: #initializeAfterLoad1) = (RBParser parseMethod: 'initializeAfterLoad1
								RSR := RBParseTreeSearcher new.
								RSR
									addMethodSearches: #(''`@methodName: `@args | `@temps | self `@methodName: `@args'' ''`@methodName: `@args | `@temps | ^self `@methodName: `@args'')
											-> [:aNode :answer | true]').
	self assert: (class theMetaClass parseTreeFor: #nuke) = (RBParser parseMethod: 'nuke
								RSR := nil').
	self assert: (class parseTreeFor: #checkMethod:) = (RBParser parseMethod: 'checkMethod: aSmalllintContext 
								class := aSmalllintContext selectedClass.
								(rewriteRule executeTree: aSmalllintContext parseTree) ifTrue: 
										[(RSR executeTree: rewriteRule tree initialAnswer: false)
											ifFalse: 
												[builder compile: rewriteRule tree printString
													in: class
													classified: aSmalllintContext protocols]]')