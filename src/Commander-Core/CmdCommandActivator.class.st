"
I responsible to execute the command instance in concrete application context according to given activation strategy. 

I define three steps to execute command:

1) #prepareCommandForExecution. Command should retrieve all state required for execution from activation context. By default ths logic is delegated to the command though context instance:
	CmdCommandActivator>>prepareCommandForExecution
		actualActivationContext prepareExecutionOf: command  
During preparation commands can break execution by signalling CmdCommandAborted. For example It should happen if user cancel some confirmation dialog during command preparation.

2) Command execution. All logic is implemented by command itself (#execute method).
	command execute.
	
3) Applying execution result to activation context. It is also delegated to the command itself though context instance:
	CmdCommandActivator>>applyCommandResult
		actualActivationContext applyResultOf: command  
Idea is to be able interact with application when command completes. For example if user creates new package from browser then at the end the browser should open created package.
 
For more details look at CmdCommand comments.

I am able to check if comand can be executed in my context:
	activator canExecuteCommand

Different packages extends me to represent commands according to concrete activation strategy. For example context menu will ask me to create menu items. In such cases I just delegate actual logic to the command itself.

Complex activation strategy can provide my subclasses. For example Drag&Drop command activation requires two steps to prepare and execute command. And there is CmdDragAndDropCommandActivator which incapsulates two active contexts where command should be executed. Look at it for details.  

My instances are created by activation strategy:
	activationStratagy newActivatorFor: aToolContext

Internal Representation and Key Implementation Points.

    Instance Variables
	command:		<CmdCommand> an activating command
	context:		<CmdToolContext>	an active context where command is activated
	activationStrategy:		<CmdCommandActivationStrategy>	strategy which defines how command should be accessed and executed in given context
"
Class {
	#name : #CmdCommandActivator,
	#superclass : #Object,
	#instVars : [
		'command',
		'context',
		'activationStrategy'
	],
	#category : #'Commander-Core'
}

{ #category : #'instance creation' }
CmdCommandActivator class >> createCommand: aCommandClass inContext: aToolContext [
	| command |
	command := aCommandClass new.
	aToolContext prepareNewCommand: command.
	^command
]

{ #category : #'instance creation' }
CmdCommandActivator class >> for: aCommandClass inContext: aToolContext using: aCommandActivationStrategy [
	| command |
	command := self createCommand: aCommandClass inContext: aToolContext.
	
	^self new 
		command: command;
		context: aToolContext;
		activationStrategy: aCommandActivationStrategy
]

{ #category : #testing }
CmdCommandActivator >> activatesCommand: aCommandClass [ 
	^activationStrategy annotates: aCommandClass
]

{ #category : #accessing }
CmdCommandActivator >> activationStrategy [
	^ activationStrategy
]

{ #category : #accessing }
CmdCommandActivator >> activationStrategy: anObject [
	activationStrategy := anObject
]

{ #category : #execution }
CmdCommandActivator >> applyCommandResult [
	context applyResultOf: command
]

{ #category : #converting }
CmdCommandActivator >> asCommandActivator [
	^self
]

{ #category : #testing }
CmdCommandActivator >> canExecuteCommand [

	^activationStrategy isActiveInContext: context
]

{ #category : #accessing }
CmdCommandActivator >> command [
	^ command
]

{ #category : #accessing }
CmdCommandActivator >> command: anObject [
	command := anObject
]

{ #category : #accessing }
CmdCommandActivator >> commandDescription [

	^command description ifNil: [ self menuItemName ]
]

{ #category : #accessing }
CmdCommandActivator >> context [
	^ context
]

{ #category : #accessing }
CmdCommandActivator >> context: anObject [
	context := anObject
]

{ #category : #execution }
CmdCommandActivator >> executeCommand [

	[self prepareCommandForExecution.
	context executeCommand: command by: self.
	self applyCommandResult] 
		on: Exception do: [ :exc | self processCommandFailure: exc ]
]

{ #category : #testing }
CmdCommandActivator >> isSimilarTo: anotherCommandActivator [

	^(activationStrategy isSimilarTo: anotherCommandActivator activationStrategy)
		and: [context isSimilarTo: anotherCommandActivator context ]
]

{ #category : #accessing }
CmdCommandActivator >> menuItemIcon [
	^self command defaultMenuIcon
]

{ #category : #accessing }
CmdCommandActivator >> menuItemName [
	^activationStrategy menuItemNameFor: command
]

{ #category : #accessing }
CmdCommandActivator >> menuItemOrder [
	^activationStrategy menuItemOrder
]

{ #category : #accessing }
CmdCommandActivator >> newForCommand: aCommand [
	
	| copy |
	copy := self copy.
	copy command: aCommand.
	^copy
]

{ #category : #execution }
CmdCommandActivator >> prepareCommandForExecution [
	context prepareFullExecutionOf: command
]

{ #category : #printing }
CmdCommandActivator >> printOn: aStream [
	super printOn: aStream.
	
	aStream nextPut: $(.
	command printOn: aStream.
	aStream nextPut: $).
]

{ #category : #execution }
CmdCommandActivator >> processCommandFailure: anException [
	
	context processFailure: anException of: command
]
