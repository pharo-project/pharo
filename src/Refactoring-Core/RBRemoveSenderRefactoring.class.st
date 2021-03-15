"
I'm a refactoring for remove a method call.

Example
-------
```
| refactoring options |
refactoring := RBRemoveSenderRefactoring 
		remove: (	40 to: 120)
		inMethod: #caller1
		forClass: RBRefactoryTestDataApp.
options := refactoring options copy.
options at: #inlineExpression put: [:ref :string | false].
refactoring options: options.
refactoring execute.
```

Before refactoring:

```
RBRefactoryTestDataApp >> caller1
	| anObject |
	anObject := 5.
	self called: anObject + 1
		on1: 
			[:each | 
			each printString.
			^anObject]
```

After refactoring:
```
RBRefactoryTestDataApp >> caller1
	| anObject |
	anObject := 5.
```

"
Class {
	#name : #RBRemoveSenderRefactoring,
	#superclass : #RBInlineMethodRefactoring,
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBRemoveSenderRefactoring class >> model: aRBSmalltalk remove: anInterval inMethod: aSelector forClass: aClass [ 
	^(self new)
		model: aRBSmalltalk;
		remove: anInterval
			inMethod: aSelector
			forClass: aClass;
		yourself
]

{ #category : #'instance creation' }
RBRemoveSenderRefactoring class >> remove: anInterval inMethod: aSelector forClass: aClass [ 
	^self new 
		remove: anInterval
		inMethod: aSelector
		forClass: aClass
]

{ #category : #transforming }
RBRemoveSenderRefactoring >> findSelectedMessage [
	sourceParseTree := class parseTreeFor: sourceSelector.
	sourceParseTree ifNil: [ self refactoringFailure: 'Could not parse sources' ].
	sourceMessage := sourceParseTree whichNodeIsContainedBy: sourceInterval.
	sourceMessage
		ifNil: [ self refactoringFailure: 'The selection doesn''t appear to be a message send' ].
	sourceMessage isDirectlyUsed
		ifTrue: [ self refactoringFailure: 'The sender is directly used' ].
	sourceMessage isCascade
		ifTrue: [ sourceMessage := sourceMessage messages last ].
		
	sourceMessage isMessage
		ifFalse: [ self refactoringFailure: 'The selection doesn''t appear to be a message send' ].
	(sourceMessage receiver isSelfVariable or: [ sourceMessage receiver isSuperVariable ])
		ifFalse: [ self refactoringError: 'Cannot inline non-self messages' ]
]

{ #category : #transforming }
RBRemoveSenderRefactoring >> inlineSourceReplacing: aParseTree [
	| statements nodeUnderSequence |
	
	statements := inlineParseTree body statements.
	(statements size > 1 and: [ aParseTree isEvaluatedFirst not ]) ifTrue: [ 
		self refactoringWarning:
			'To inline this method, we need to move some of its statements before the original message send.<n>This could change the order of execution, which can change the behavior.<n>Do you want to proceed?'
				expandMacros ].
	nodeUnderSequence := aParseTree.
	[ nodeUnderSequence parent isSequence ] whileFalse: [ nodeUnderSequence := nodeUnderSequence parent ].
	aParseTree parent replaceNode: aParseTree withNode: RBVariableNode selfNode
]

{ #category : #removing }
RBRemoveSenderRefactoring >> remove: anInterval inMethod: aSelector forClass: aClass [ 
	sourceSelector := aSelector.
	class := self classObjectFor: aClass.
	sourceInterval := anInterval
]
