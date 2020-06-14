"
An AbstractCommand is an abstract class. It is responsible for all commands that create metavariable.
Method abstractWithName: andIndex: is responsible for creating metavariables.

"
Class {
	#name : #AbstractCommand,
	#superclass : #Command,
	#category : #'RewriteTool-Command'
}

{ #category : #accessing }
AbstractCommand >> abstractName [
	self subclassResponsibility
]

{ #category : #execute }
AbstractCommand >> abstractWithName: aString andIndex: anInteger [
	| newNode |
	newNode := self panel
		chooseMetavariablesForNode: self commandReceiver.
	newNode = 0
		ifTrue: [ ^ self ].
	newNode
		ifNil:
			[ newNode := RBPatternVariableNode named: aString , anInteger asString ].
	self changeCommandReceiverFor: newNode.
	^ newNode
]

{ #category : #accessing }
AbstractCommand >> length [
	^ self abstractName size - 1
]
