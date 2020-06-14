"
An AbstractStatementCommand is command that creates `.Statement metavariable.

"
Class {
	#name : #AbstractStatementCommand,
	#superclass : #AbstractCommand,
	#category : #'RewriteTool-Command'
}

{ #category : #accessing }
AbstractStatementCommand >> abstractName [
	^ '`.@Statement'
]

{ #category : #execute }
AbstractStatementCommand >> abstractSequenceStatement [
	| newNode temps |
	temps := '| '.
	self commandReceiver temporaries do: [ :each | temps := temps , each name , String space ].
	temps := temps , ' | '.
	newNode := self panel chooseMetavariablesForNode: self commandReceiver.
	newNode
		ifNil: [ newNode := RBPatternVariableNode named: temps , self abstractName , self panel statementIndex asString ].
	self commandReceiver replaceWith: newNode.
	^ newNode
]

{ #category : #execute }
AbstractStatementCommand >> execute [
	^ self abstractWithName: self abstractName andIndex: self panel statementIndex
]
