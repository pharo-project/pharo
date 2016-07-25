tests
testAbstractClassVariable
	| refactoring meta class |
	refactoring := RBAbstractClassVariableRefactoring 
		variable: 'RecursiveSelfRule'
		class: RBTransformationRuleTest.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBTransformationRuleTest.
	meta := class theMetaClass.
	self assert: (meta parseTreeFor: #recursiveSelfRule) = (RBParser parseMethod: 'recursiveSelfRule ^RecursiveSelfRule').
	self assert: (meta parseTreeFor: #recursiveSelfRule:) = (RBParser parseMethod: 'recursiveSelfRule: anObject RecursiveSelfRule := anObject').
	self assert: (meta parseTreeFor: #nuke) = (RBParser parseMethod: 'nuke
							self recursiveSelfRule: nil').
	self assert: (meta parseTreeFor: #initializeAfterLoad1) = (RBParser parseMethod: 'initializeAfterLoad1
							self recursiveSelfRule: RBParseTreeSearcher new.
							self recursiveSelfRule
								addMethodSearches: #(''`@methodName: `@args | `@temps | self `@methodName: `@args'' ''`@methodName: `@args | `@temps | ^self `@methodName: `@args'')
										-> [:aNode :answer | true]').
	self assert: (class parseTreeFor: #checkMethod:) = (RBParser parseMethod: 'checkMethod: aSmalllintContext 
							class := aSmalllintContext selectedClass.
							(rewriteRule executeTree: aSmalllintContext parseTree) ifTrue: 
									[(self class recursiveSelfRule executeTree: rewriteRule tree initialAnswer: false)
										ifFalse: 
											[builder compile: rewriteRule tree printString
												in: class
												classified: aSmalllintContext protocols]]')