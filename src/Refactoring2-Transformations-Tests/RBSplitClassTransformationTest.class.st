Class {
	#name : #RBSplitClassTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #testing }
RBSplitClassTransformationTest >> testClassVariable [

	self shouldFail: (RBSplitClassTransformation
							class:  #RBDummyLintRuleTest
							instanceVariables: #(Foo1)
							newClassName: #RBDummyLintRuleTest123
							referenceVariableName: #receiver)
							asRefactoring.
]

{ #category : #testing }
RBSplitClassTransformationTest >> testNonExistantName [

	self shouldFail: (RBSplitClassTransformation
							class: #RBDummyLintRuleTest 
							instanceVariables: #(name1)
							newClassName: #RBDummyLintRuleTest123
							referenceVariableName: #newName)
							asRefactoring
]

{ #category : #testing }
RBSplitClassTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBSplitClassTransformation
							class: #RBRemoveDirectAccessToVariableTransformation
							instanceVariables: #(receiver)
							newClassName: #RBRemoveDirectAccessWithReceiverTransformation
							referenceVariableName: #newReceiver)
							transform.
		
	class := transformation model classNamed: #RBRemoveDirectAccessToVariableTransformation.
	self deny: (class instanceVariableNames includes: #receiver).
	self assert: (class instanceVariableNames includes: #newReceiver).
	
	self assert: (class parseTreeFor: #receiver:)
		  equals: (self parseMethod:
				'receiver: aString
					newReceiver receiver: aString').
					
	self assert: (class parseTreeFor: #receiver)
		  equals: (self parseMethod:
				'receiver
					^ newReceiver receiver 
						ifNil: [ self receiver: ', 'self' surroundedBySingleQuotes,
						'. newReceiver receiver ]').				
	
	class := transformation model classNamed: #RBRemoveDirectAccessWithReceiverTransformation.
	self deny: class isNil.
	self assert: (class instanceVariableNames includes: #receiver).
	self assert: (class directlyDefinesLocalMethod: #receiver).
	self assert: (class directlyDefinesLocalMethod: #receiver:)
]
