"
An AsListCommand is command that abstracts metavariable `variable into `@variable.

"
Class {
	#name : #AsListCommand,
	#superclass : #Command,
	#category : #'RewriteTool-Command'
}

{ #category : #accessing }
AsListCommand >> abstractName [
	^ nil
]

{ #category : #execute }
AsListCommand >> execute [
	| newName newNode |
	newName := commandReceiver recurseInto
		ifTrue: [ '``@' , (commandReceiver name allButFirst: 2) ]
		ifFalse: [ '`@' , commandReceiver name allButFirst ].
	newNode := RBPatternVariableNode named: newName.
	self changeCommandReceiverFor:  newNode.
	^ newNode
]

{ #category : #accessing }
AsListCommand >> length [
	^ commandReceiver formattedCode size - 1
]
