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

{ #category : 'execution' }
SycRemoveMessageArgumentCommand >> applyResultInContext: aToolContext [
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

{ #category : 'accessing' }
SycRemoveMessageArgumentCommand >> defaultMenuIconName [
	^#remove
]

{ #category : 'accessing' }
SycRemoveMessageArgumentCommand >> defaultMenuItemName [
	^'Remove argument'
]

{ #category : 'execution' }
SycRemoveMessageArgumentCommand >> execute [
	
	ReRemoveParameterDriver new
		scopes: refactoringScopes
			model: model
			removeParameterFromMethodSignature: originalMessage
			in: originalMessage methodClass;
		runRefactoring
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
SycRemoveMessageArgumentCommand >> resultMessageSelector [
	^ self methodName selector
]
