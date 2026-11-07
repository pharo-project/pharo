"
I am a command to remove existing argument from the message.

Internal Representation and Key Implementation Points.

    Instance Variables
	argumentName:		<String>
"
Class {
	#name : 'SycRemoveMessageArgumentCommand',
	#superclass : 'SycMessageCmCommand',
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
	CmCommandAborted signal
]

{ #category : 'execution' }
SycRemoveMessageArgumentCommand >> applyCommandResult [
	"The refactoring was applied by the driver while executing the command."

	
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
	"not used"
]

{ #category : 'execution' }
SycRemoveMessageArgumentCommand >> executeRefactoring [

	ReRemoveParameterDriver new
		scopes: refactoringScopes
		model: model
		removeParameterFromMethodSignature: originalMessage
		in: originalMessage methodClass;
		runRefactoring
]

{ #category : 'accessing' }
SycRemoveMessageArgumentCommand >> icon [

	^ self iconNamed: #remove
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

{ #category : 'accessing' }
SycRemoveMessageArgumentCommand >> name [

	^ 'Remove argument'
]

{ #category : 'accessing' }
SycRemoveMessageArgumentCommand >> order [

	^ 40
]

{ #category : 'execution' }
SycRemoveMessageArgumentCommand >> resultMessageSelector [
	^ self methodName selector
]
