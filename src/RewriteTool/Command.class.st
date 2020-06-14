"
A Command is an abstract class. It is built by Command pattern.

Instance Variables
	application:		<RewriteRuleBuilder>
	commandReceiver:		<RBProgramNode>
	index:		<Integer>
	newSource:		<String>
	panel:		<AbstractPanel>

application
	- an instance of RewriteRuleBuilder

commandReceiver
	- an ast nodem that recieves this command

index
	- used for choosing item with UIManager

newSource
	- source that is used in commands as ChangeCommand, ChangeSelectorCommand

panel
	- panel of RewriteRuleBuilder in which this command was invoked

"
Class {
	#name : #Command,
	#superclass : #Object,
	#instVars : [
		'commandReceiver',
		'newSource',
		'panel'
	],
	#category : #'RewriteTool-Command'
}

{ #category : #testing }
Command class >> isAccepted: aNode [
	^ true
]

{ #category : #testing }
Command class >> isSequenceNeeded [
	^ false
]

{ #category : #testing }
Command class >> isTemps [
	^ false
]

{ #category : #'instance creation' }
Command class >> receiver: aReceiver panel: aPanel withSource: aString [
	^ self new
		commandReceiver: aReceiver;
		panel: aPanel;
		newSource: aString;
		yourself
]

{ #category : #execute }
Command >> change: aNode For: anotherNode [
	self panel isSelectedAll
		ifTrue: [ self panel textModel ast: anotherNode ]
		ifFalse: [ aNode replaceWith: anotherNode ]
]

{ #category : #execute }
Command >> changeCommandReceiverFor: aNode [
	self change: self commandReceiver For: aNode
]

{ #category : #accessing }
Command >> commandReceiver [
	^ commandReceiver
]

{ #category : #accessing }
Command >> commandReceiver: anObject [
	commandReceiver := anObject
]

{ #category : #execute }
Command >> execute [
	self subclassResponsibility
]

{ #category : #accessing }
Command >> newSource [
	^ newSource
]

{ #category : #accessing }
Command >> newSource: anObject [
	newSource := anObject
]

{ #category : #accessing }
Command >> panel [
	^ panel
]

{ #category : #accessing }
Command >> panel: anObject [
	panel := anObject
]
