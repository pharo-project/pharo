Class {
	#name : #RBRenameVariableTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #testing }
RBRenameVariableTransformationTest >> testClassVariable [

	| refactoring class |
	refactoring := (RBRenameVariableTransformation
						rename: #RecursiveSelfRule to: #RSR
						in: #RBTransformationRuleTestData
						classVariable: true)
						asRefactoring transform.
	
	class := refactoring model classNamed: #RBTransformationRuleTestData.
	self assert: (class directlyDefinesClassVariable: #RSR).
	self deny: (class directlyDefinesClassVariable: #RecursiveSelfRule).
	self assert: (class theMetaClass parseTreeFor: #initializeAfterLoad1) 
		  equals: (self parseMethod: 
				'initializeAfterLoad1
					RSR := RBParseTreeSearcher new.
					RSR addMethodSearches: #(''`@methodName: `@args | `@temps | self `@methodName: `@args'' ''`@methodName: `@args | `@temps | ^self `@methodName: `@args'')
						-> [:aNode :answer | true]').
	self assert: (class theMetaClass parseTreeFor: #nuke) 
		  equals: (self parseMethod: 'nuke RSR := nil').
	self assert: (class parseTreeFor: #checkMethod:) 
		  equals: (self parseMethod: 
				'checkMethod: aSmalllintContext 
					class := aSmalllintContext selectedClass.
					(rewriteRule executeTree: aSmalllintContext parseTree) ifTrue: 
						[(RSR executeTree: rewriteRule tree initialAnswer: false)
						ifFalse: [builder compile: rewriteRule tree printString
										in: class
										classified: aSmalllintContext protocols]]')
]

{ #category : #testing }
RBRenameVariableTransformationTest >> testMetaclassFailure [

	self shouldFail: (RBRenameVariableTransformation
							rename: #RecursiveSelfRule to: #Foo
							in: RBTransformationRuleTestData class
							classVariable: true)
							asRefactoring
]

{ #category : #testing }
RBRenameVariableTransformationTest >> testRefactoring [

	| refactoring class |
	refactoring := (RBRenameVariableTransformation 
						rename: 'classBlock' to: 'asdf'
						in: #RBBasicLintRuleTestData
						classVariable: false)
						asRefactoring transform.
			
	class := refactoring model classNamed: #RBBasicLintRuleTestData.
	self assert: (class directlyDefinesInstanceVariable: 'asdf').
	self deny: (class directlyDefinesInstanceVariable: 'classBlock').
	self assert: (class parseTreeFor: #checkClass:)
		  equals: (self parseMethod:
				'checkClass: aSmalllintContext 
					^asdf value: aSmalllintContext value: result').
	self assert: (class parseTreeFor: #initialize)
		  equals: (self parseMethod:
				'initialize
					super initialize.
					self anInstVar: 1.
					asdf := [:context :aResult | ].
					methodBlock := [:context :aResult | ].
					self resultClass: RBSelectorEnvironment')
]

{ #category : #testing }
RBRenameVariableTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBRenameVariableTransformation
							rename: 'classBlock' to: 'asdf'
							in: #RBBasicLintRuleTestData
							classVariable: false)
							transform.
	
	class := transformation model classNamed: #RBBasicLintRuleTestData.
	self assert: (class directlyDefinesInstanceVariable: 'asdf').
	self deny: (class directlyDefinesInstanceVariable: 'classBlock').
	self assert: (class parseTreeFor: #checkClass:)
		  equals: (self parseMethod:
				'checkClass: aSmalllintContext 
					^asdf value: aSmalllintContext value: result').
	self assert: (class parseTreeFor: #initialize)
		  equals: (self parseMethod:
				'initialize
					super initialize.
					self anInstVar: 1.
					asdf := [:context :aResult | ].
					methodBlock := [:context :aResult | ].
					self resultClass: RBSelectorEnvironment')
]

{ #category : #testing }
RBRenameVariableTransformationTest >> testVariableAlreadyExists [

	self
		shouldFail: (RBRenameVariableTransformation 
						rename: 'classBlock' to: 'name'
						in: #RBBasicLintRuleTestData
						classVariable: false)
						asRefactoring;
		shouldFail: (RBRenameVariableTransformation 
						rename: #RecursiveSelfRule to: self objectClassVariable
						in: #RBTransformationRuleTestData
						classVariable: true)
						asRefactoring
]

{ #category : #testing }
RBRenameVariableTransformationTest >> testVariableDoesNotExist [

	self
		shouldFail: (RBRenameVariableTransformation 
						rename: 'foo' to: 'newFoo'
						in: #RBBasicLintRuleTestData
						classVariable: false)
						asRefactoring;
		shouldFail: (RBRenameVariableTransformation 
						rename: #foo to: #newFoo
						in: #RBBasicLintRuleTestData
						classVariable: true)
						asRefactoring
]
