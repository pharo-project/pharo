"
I am a root of command activation strategy hierarchy. My subclasses represent different ways how to access and execute commands in specific context, how to represent them to user, how initialize their state.

Users attach concrete activation strategy to commands as class annotation (look at ClassAnnotation comment).
For example to make YourCommand executable by shortcut you need following method:
	YourCommand class>>yourAppShortcutActivator
		<classAnnotation>
		^CmdShortcutCommandActivator by: $e meta for: YourAppContext 

My instances should be created for concrete context of application tool, subclass of CmdToolContext. Application/tool provides instance of this context for the command lookup:
	CmdShortcutCommandActivation allAvailableInContext: aToolContext do: [:activationStrategy  | ]
This method enumerates all registered strategies which are declared to be used in given context. You can check it  manually: 
	anActivationStrategy canBeUsedInContext: aToolContext
There is another enumaration method to access all strategies which are able execute commands in specified context:
	CmdShortcutCommandActivation allExecutableInContext: aToolContext do: [:activationStrategy  | ]
Each command defines the method which checks that given context is appropriate for command execution:
	commandClass canBeExecutedInContext: aToolContext.
Also you can ask the activation strategy about it:
	anActivationStrategy canExecuteCommandInContext: aToolContext

My instances itself are not supposed to execute commands. For this purpose you  need an activator instances:
	activator := anActivationStrategy newActivatorFor: aToolContext
The activator instance incapsulates the current context where command should be executed. And you can work with it as a self containt object.
You can check if activator can execute the command:
	activator canExecuteCommand
You can execute the command:
	activator executeCommand
Internally activator creates command instance and initializes it with the state related to the current context. 
But actual logic of these operations is implemented by command itself. So activator delegates it to the command:
	command readParametersFromContext:  context 
	command prepareFullExecutionInContext: context
	command execute.
Initialization logic of the command depends on the type of activation strategy. For example drag and drop activation will require two steps to prepare command:
	command prepareExecutionInDragContext: aToolContext
It will initialize the state of command which is available from the context of drag operation.
	command prepareExecutionInDropContext: aToolContext
It will initialize the state of command in the context of drop operation.

Look at CmdCommand and CmdCommandActivator for details on these logic.

I also provide simple method to work with all commands using activators prepared for the given context:
	activationStrategyClass activateAllInContext: aToolContext by: [:activator | ]
Also you can collect all these activators:
	activationStrategyClass creatActivatorsExecutableInContext: aToolContext
	
Internal Representation and Key Implementation Points.

    Instance Variables
	contextDefinition:		<CmdContextDefinition>	definition of the context where command can be activated
"
Class {
	#name : #CmdCommandActivationStrategy,
	#superclass : #ClassAnnotation,
	#category : #'Commander-Core'
}

{ #category : #activation }
CmdCommandActivationStrategy class >> activateAllInContext: aToolContext by: blockWithActivator [

	self 
		activeInstancesInContext: aToolContext 
		do: [:activationStrategy | 
			blockWithActivator value: (activationStrategy newActivatorFor: aToolContext)]
]

{ #category : #activation }
CmdCommandActivationStrategy class >> createActivatorsExecutableInContext: aToolContext [
	| result |
	result := OrderedCollection new.
	
	self 
		activateAllInContext: aToolContext 
		by: [:each | result add: each].
		
	^result
]

{ #category : #accessing }
CmdCommandActivationStrategy >> commandClass [
	^annotatedClass
]

{ #category : #activation }
CmdCommandActivationStrategy >> defaultActivatorClass [
	^CmdCommandActivator
]

{ #category : #testing }
CmdCommandActivationStrategy >> isActiveInContext: aToolContext [
	^(super isActiveInContext: aToolContext)
		and: [ aToolContext allowsExecutionOf: self commandClass]
]

{ #category : #testing }
CmdCommandActivationStrategy >> isForbidden [
	^self commandClass isAbstract
]

{ #category : #activation }
CmdCommandActivationStrategy >> newActivatorFor: aToolContext [
	
	^self defaultActivatorClass
		for: self commandClass
		inContext: aToolContext 
		using: self
]
