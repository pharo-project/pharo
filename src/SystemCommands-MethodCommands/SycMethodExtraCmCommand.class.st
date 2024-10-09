"
This class is an extension so we can mix command of Commander2 with Commander.
"
Class {
	#name : 'SycMethodExtraCmCommand',
	#superclass : 'SycCmCommand',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'adding' }
SycMethodExtraCmCommand class >> activationStrategy [
	^ SycExtraMenuActivation
]

{ #category : 'testing' }
SycMethodExtraCmCommand >> canBeExecuted [ 

	^ self methods notNil and: [ self methods notEmpty ]
]

{ #category : 'accessing' }
SycMethodExtraCmCommand >> methods [
	"Private - Answer a <Collection> of context's selected <CompiledMethod>"

	^ context selectedMethods 
]

{ #category : 'preparation' }
SycMethodExtraCmCommand >> prepareFullExecution [
	super prepareFullExecution.
	self setUpModelFromContext: context
]
