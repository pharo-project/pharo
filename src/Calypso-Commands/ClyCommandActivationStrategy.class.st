"
I am the root of the Calypso command activation strategies. I describe how a command is reached in a given context, for example from a table icon. Concrete subclasses are attached to a command as a class annotation, and they build the activators that execute the command.
"
Class {
	#name : 'ClyCommandActivationStrategy',
	#superclass : 'ClassAnnotation',
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'activation' }
ClyCommandActivationStrategy class >> activateAllInContext: aToolContext by: blockWithActivator [

	self
		activeInstancesInContext: aToolContext
		do: [:activationStrategy |
			blockWithActivator value: (activationStrategy newActivatorFor: aToolContext)]
]

{ #category : 'activation' }
ClyCommandActivationStrategy class >> createActivatorsExecutableInContext: aToolContext [
	| result |
	result := OrderedCollection new.

	self
		activateAllInContext: aToolContext
		by: [:each | result add: each].

	^result
]

{ #category : 'testing' }
ClyCommandActivationStrategy class >> isAbstract [

	^ self == ClyCommandActivationStrategy
]

{ #category : 'accessing' }
ClyCommandActivationStrategy >> commandClass [
	^annotatedClass
]

{ #category : 'activation' }
ClyCommandActivationStrategy >> defaultActivatorClass [
	^ ClyCommandActivator
]

{ #category : 'testing' }
ClyCommandActivationStrategy >> isActiveInContext: aToolContext [
	^(super isActiveInContext: aToolContext)
		and: [ aToolContext allowsExecutionOf: self commandClass]
]

{ #category : 'testing' }
ClyCommandActivationStrategy >> isForbidden [
	^self commandClass isAbstract
]

{ #category : 'activation' }
ClyCommandActivationStrategy >> newActivatorFor: aToolContext [

	^self defaultActivatorClass
		for: self commandClass
		inContext: aToolContext
		using: self
]
