Class {
	#name : #RBRenameClassVariableTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #'failure tests' }
RBRenameClassVariableTest >> testAlreadyExistingName [
	self shouldFail: (RBRenameClassVariableRefactoring 
			rename: #RecursiveSelfRule
			to: self objectClassVariable
			in: RBTransformationRuleTestData)
]

{ #category : #'failure tests' }
RBRenameClassVariableTest >> testMetaClassFailure [
	self shouldFail: (RBRenameClassVariableRefactoring 
			rename: #RecursiveSelfRule
			to: #Foo
			in: RBTransformationRuleTestData class)
]

{ #category : #'failure tests' }
RBRenameClassVariableTest >> testNonExistantName [
	self shouldFail: (RBRenameClassVariableRefactoring 
			rename: #foo
			to: #newFoo
			in: RBBasicLintRuleTestData)
]

{ #category : #tests }
RBRenameClassVariableTest >> testRenameClassVar [
	| refactoring class |
	refactoring := RBRenameClassVariableRefactoring rename: #RecursiveSelfRule to: #RSR in: RBTransformationRuleTestData.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBTransformationRuleTestData.
	self assert: (class directlyDefinesClassVariable: #RSR).
	self deny: (class directlyDefinesClassVariable: #RecursiveSelfRule).
	self
		assert: (class classSide parseTreeFor: #initializeAfterLoad1)
		equals:
			(self
				parseMethod:
					'initializeAfterLoad1
								RSR := RBParseTreeSearcher new.
								RSR
									addMethodSearches: #(''`@methodName: `@args | `@temps | self `@methodName: `@args'' ''`@methodName: `@args | `@temps | ^self `@methodName: `@args'')
											-> [:aNode :answer | true]').
	self
		assert: (class classSide parseTreeFor: #nuke)
		equals:
			(self
				parseMethod:
					'nuke
								RSR := nil').
	self
		assert: (class parseTreeFor: #checkMethod:)
		equals:
			(self
				parseMethod:
					'checkMethod: aSmalllintContext 
								class := aSmalllintContext selectedClass.
								(rewriteRule executeTree: aSmalllintContext parseTree) ifTrue: 
										[(RSR executeTree: rewriteRule tree initialAnswer: false)
											ifFalse: 
												[builder compile: rewriteRule tree printString
													in: class
													classified: aSmalllintContext protocols]]')
]

{ #category : #tests }
RBRenameClassVariableTest >> testRenameClassVarInSharedPool [
	| refactoring class userClass |
	refactoring := RBRenameClassVariableRefactoring 
		rename: #Var1
		to: #VarOne
		in: RBSharedPoolForTestData.
	self executeRefactoring: refactoring.
	
	class := refactoring model classNamed: #RBSharedPoolForTestData.
	userClass := refactoring model classNamed: #RBClassUsingSharedPoolForTestData.
	
	self assert: (class parseTreeFor: #msg1) equals: (self parseMethod: 'msg1 ^ VarOne').	
	self assert: (class parseTreeFor: #msg2) equals: (self parseMethod: 'msg2 ^ VarOne').	
	self assert: (userClass parseTreeFor: #msg3) equals: (self parseMethod: 'msg3 ^ VarOne').	

]
