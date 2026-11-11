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

{ #category : 'command registry' }
ClyCommandActivationStrategy class >> interactionName [
	"The interaction of ClyCommandMap whose declared commands I answer with my annotations, or nil."

	^ nil
]

{ #category : 'testing' }
ClyCommandActivationStrategy class >> isAbstract [

	^ self == ClyCommandActivationStrategy
]

{ #category : 'command registry' }
ClyCommandActivationStrategy class >> registeredInstances [
	"My class annotations followed by the commands declared for my interaction with the <calypsoCommands> pragma (see ClyCommandRegistry). Every way Calypso enumerates activations goes through me, so it shows both while commands move from class annotations to declarations."

	| annotations |
	annotations := super registeredInstances.
	^ self interactionName
		  ifNil: [ annotations ]
		  ifNotNil: [ :interactionName | annotations asArray , (ClyCommandRegistry default allEntriesOf: interactionName) ]
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
