"
A RecurseIntoCommand is command that abstracts metavariable `@variable into ``@variable.

"
Class {
	#name : #RecurseIntoCommand,
	#superclass : #Command,
	#category : #'RewriteTool-Command'
}

{ #category : #accessing }
RecurseIntoCommand >> abstractName [
	^ nil
]

{ #category : #execute }
RecurseIntoCommand >> execute [
	| newNode |
	newNode := RBPatternVariableNode named: '`' , commandReceiver formattedCode.
	self changeCommandReceiverFor: newNode.
	^ newNode
]

{ #category : #accessing }
RecurseIntoCommand >> length [
	^ commandReceiver formattedCode size - 1
]
