Class {
	#name : 'RBRenameClassVariableParametrizedTest',
	#superclass : 'RBRenameVariableParametrizedTest',
	#category : 'Refactoring-Transformations-Tests-Parametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'building suites' }
RBRenameClassVariableParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBRenameClassVariableRefactoring .
					  #constructor -> #rename:to:in: .
					  #extraArgument -> #() };
		addCase: { #rbClass -> RBRenameVariableTransformation .
					  #constructor -> #rename:to:in:classVariable: .
					  #extraArgument -> #(true) };
		yourself
]

{ #category : 'failure tests' }
RBRenameClassVariableParametrizedTest >> testFailureAlreadyExistingName [

	self shouldFail: (self createRefactoringWithArguments: {
				 #RecursiveSelfRule.
				 self objectClassVariable.
				 #RBTransformationRuleTestData })
]

{ #category : 'failure tests' }
RBRenameClassVariableParametrizedTest >> testFailureMetaClassFailure [

	self shouldFail: (self createRefactoringWithArguments: {
				 #RecursiveSelfRule.
				 #Foo.
				 RBTransformationRuleTestData class name asSymbol })
]

{ #category : 'failure tests' }
RBRenameClassVariableParametrizedTest >> testFailureNonExistantName [

	self shouldFail: (self createRefactoringWithArguments:
			 { #foo. #newFoo. #RBBasicLintRuleTestData })
]

{ #category : 'tests' }
RBRenameClassVariableParametrizedTest >> testRenameClassVar [
	| refactoring class |
	refactoring := self createRefactoringWithArguments:
		{ #RecursiveSelfRule .
		#RSR .
		#RBTransformationRuleTestData}.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBTransformationRuleTestData.
	self assert: (class directlyDefinesClassVariable: #RSR).
	self deny: (class directlyDefinesClassVariable: #RecursiveSelfRule).
	self
		assert: (class classSide parseTreeForSelector: #initializeAfterLoad1)
		equals:
			(self
				parseMethod:
					'initializeAfterLoad1
								RSR := self parseTreeSearcher.
								RSR
									addMethodSearches: #(''`@methodName: `@args | `@temps | self `@methodName: `@args'' ''`@methodName: `@args | `@temps | ^self `@methodName: `@args'')
											-> [:aNode :answer | true]').
	self
		assert: (class classSide parseTreeForSelector: #nuke)
		equals:
			(self
				parseMethod:
					'nuke
								RSR := nil').
	self
		assert: (class parseTreeForSelector: #checkMethod:)
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

{ #category : 'tests' }
RBRenameClassVariableParametrizedTest >> testRenameClassVarInSharedPool [
	| refactoring class userClass |

	refactoring := self createRefactoringWithArguments:
		{ #Var1 .
		#VarOne .
		#RBSharedPoolForTestData}.

	self executeRefactoring: refactoring.

	class := refactoring model classNamed: #RBSharedPoolForTestData.
	userClass := refactoring model classNamed: #RBClassUsingSharedPoolForTestData.

	self assert: (class parseTreeForSelector: #msg1) equals: (self parseMethod: 'msg1 ^ VarOne').
	self assert: (class parseTreeForSelector: #msg2) equals: (self parseMethod: 'msg2 ^ VarOne').
		self flag: 'TODO : fix in tranform class'
"	self assert: (userClass parseTreeFor: #msg3) equals: (self parseMethod: 'msg3 ^ VarOne').	"
]
