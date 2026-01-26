"
I am a command to remove existing argument from the message.

Internal Representation and Key Implementation Points.

    Instance Variables
	argumentName:		<String>
"
Class {
	#name : 'SycRemoveMessageArgumentCommand',
	#superclass : 'SycChangeMessageSignatureCommand',
	#instVars : [
		'argumentName',
		'methodName'
	],
	#category : 'SystemCommands-MessageCommands',
	#package : 'SystemCommands-MessageCommands'
}

{ #category : 'testing' }
SycRemoveMessageArgumentCommand class >> canBeExecutedInContext: aToolContext [
	^(super canBeExecutedInContext: aToolContext)
		and: [ aToolContext lastSelectedMessage argumentNames notEmpty]
]

{ #category : 'private' }
SycRemoveMessageArgumentCommand >> abort [
	CmdCommandAborted signal
]

{ #category : 'accessing' }
SycRemoveMessageArgumentCommand >> argumentName [
	^ argumentName
]

{ #category : 'accessing' }
SycRemoveMessageArgumentCommand >> argumentName: anObject [
	argumentName := anObject
]

{ #category : 'private' }
SycRemoveMessageArgumentCommand >> computeArgumentName [

	^ (originalMessage argumentNames reject: [ :arg |
		   (self methodName arguments indexOf: arg) > 0 ]) first
]

{ #category : 'execution' }
SycRemoveMessageArgumentCommand >> createRefactoring [
	
	^(RBRemoveParameterRefactoring
		model: model
		removeParameter: argumentName
		in: refactoredClass
		selector: originalMessage selector)
		newSelector: self resultMessageSelector
]

{ #category : 'accessing' }
SycRemoveMessageArgumentCommand >> defaultMenuIconName [
	^#remove
]

{ #category : 'accessing' }
SycRemoveMessageArgumentCommand >> defaultMenuItemName [
	^'Remove argument'
]

{ #category : 'private' }
SycRemoveMessageArgumentCommand >> informArgumentError [
	self inform:
				'Argument ' , self argumentName , ' not found in method '''
				, originalMessage selector , ''''
]

{ #category : 'accessing' }
SycRemoveMessageArgumentCommand >> methodName [

	^ methodName ifNil: [
		  methodName := RBMethodName
			                selector: originalMessage selector
			                arguments: originalMessage argumentNames ]
]

{ #category : 'execution' }
SycRemoveMessageArgumentCommand >> prepareFullExecutionInContext: aToolContext [

	| dialog |
	super prepareFullExecutionInContext: aToolContext.
	dialog := SycMethodArgumentsRemoverPresenter openOn: self methodName.
	dialog cancelled ifTrue: [ self abort ].

	self argumentName: self computeArgumentName.

	(originalMessage argumentNames includes: argumentName)
		ifTrue: [ ^ self ].
	
	self informArgumentError.
	self abort
]

{ #category : 'execution' }
SycRemoveMessageArgumentCommand >> resultMessageSelector [
	^ self methodName selector
]
