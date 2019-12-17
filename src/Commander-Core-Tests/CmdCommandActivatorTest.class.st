Class {
	#name : #CmdCommandActivatorTest,
	#superclass : #TestCase,
	#category : #'Commander-Core-Tests'
}

{ #category : #tests }
CmdCommandActivatorTest >> testConvertingCommandToActivator [

	| activator command |
	command := CmdCommandExampleInRootMenu new.
	
	activator := command asCommandActivator.
	
	self assert: activator class equals: CmdCommandActivator.
	self assert: activator command equals: command.
	self assert: activator context class equals: CmdNullToolContext.
	self assert: activator activationStrategy class equals: CmdManualActivationStrategy.
	self assert: activator activationStrategy commandClass equals: command class.
]

{ #category : #tests }
CmdCommandActivatorTest >> testConvertingToActivator [
	| activator |
	activator := CmdCommandActivator new.

	self assert: activator asCommandActivator identicalTo: activator
]

{ #category : #tests }
CmdCommandActivatorTest >> testCreationNewForNewCommand [
	| activator newActivator |
	activator := CmdCommandActivator new.
	activator
		command: #command;
		context: #context;
		activationStrategy: #activation.

	newActivator := activator newForCommand: #newCommand.

	self deny: newActivator identicalTo: activator.
	self assert: newActivator class equals: activator class.
	self assert: newActivator command equals: #newCommand.
	self assert: newActivator context equals: #context.
	self assert: newActivator activationStrategy equals: #activation
]
