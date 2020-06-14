"
An AbstractStatementsCommand is command that creates `@.Statements metavariable.

"
Class {
	#name : #AbstractStatementsCommand,
	#superclass : #Command,
	#category : #'RewriteTool-Command'
}

{ #category : #accessing }
AbstractStatementsCommand >> allStatements [
	| allStatements |
	allStatements := OrderedCollection new.
	allStatements addAll: self statementsBefore.
	allStatements add: (RBPatternVariableNode named: self newSource).
	allStatements addAll: self statementsAfter.
	^ allStatements
]

{ #category : #execute }
AbstractStatementsCommand >> execute [
	| newNode chosenNode |
	self newSource: '``@.Statements' , self panel statementIndex asString.
	chosenNode := self panel chooseMetavariablesForNode: self commandReceiver.
	chosenNode ifNotNil: [ self newSource: chosenNode formattedCode ].
	newNode := RBSequenceNode temporaries: self commandReceiver temporaries statements: self allStatements.
	self changeCommandReceiverFor:  newNode.
	^ newNode
]

{ #category : #accessing }
AbstractStatementsCommand >> statementsAfter [
	^ self commandReceiver statements
		select: [ :each | 
			((self panel textModel selectionInterval rangeIncludes: each start)
				or: [ self panel textModel getSelection rangeIncludes: each stop ])
				not and: [ each start > self panel textModel getSelection last ] ]
]

{ #category : #accessing }
AbstractStatementsCommand >> statementsBefore [
	^ self commandReceiver statements
		select: [ :each | 
			((self panel textModel selectionInterval rangeIncludes: each start)
				and: [ self panel textModel selectionInterval rangeIncludes: each stop ])
				not and: [ each stop < self panel textModel getSelection first ] ]
]
