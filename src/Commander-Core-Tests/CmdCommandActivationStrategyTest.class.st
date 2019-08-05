Class {
	#name : #CmdCommandActivationStrategyTest,
	#superclass : #TestCase,
	#category : #'Commander-Core-Tests'
}

{ #category : #tests }
CmdCommandActivationStrategyTest >> testCreationNewActivatorForGivenContext [
	| activation context actual |
	activation := CmdCommandActivationExample withAnnotatedClass: CmdCommandExampleInheritedFromAbstract.
	context := CmdToolContextStub new.
	
	actual := activation newActivatorFor: context.
	
	self assert: actual class equals: CmdCommandActivator.
	self assert: actual activationStrategy equals: activation.
	self assert: actual context equals: context.
	self assert: actual command class equals: CmdCommandExampleInheritedFromAbstract.
	self assert: actual command savedParametersContext equals: context
]

{ #category : #tests }
CmdCommandActivationStrategyTest >> testIteratingInstancesShouldSkipAbstractCommands [

	| activators context |
	context := CmdToolContextStub1 new.
	activators := OrderedCollection new.
	CmdCommandActivationExample  
		visibleInstancesInContext: context
		do: [:activator | activators add: activator].
	
	self assert: activators size equals: 1.
	self assert: activators first commandClass equals: CmdCommandExampleInheritedFromAbstract
]
