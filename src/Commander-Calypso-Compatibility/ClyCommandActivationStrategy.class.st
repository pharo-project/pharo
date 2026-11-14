"
Deprecated: I will be removed in Pharo 16 with the Commander-Calypso-Compatibility package. Declare commands with the <calypsoCommands> pragma instead, see ClyCommandMap and the guide doc/Calypso/Commands.md of the Pharo repository. The migration from class annotations is explained in doc/Calypso/CommandAnnotationsMigration.md of the Pharo repository and ClyCommandDeclarationGenerator converts them.

I am the root of the Calypso command activation strategies. I describe how a command is reached in a given context, for example from a table icon. Concrete subclasses are attached to a command as a class annotation, and they build the activators that execute the command.
"
Class {
	#name : 'ClyCommandActivationStrategy',
	#superclass : 'ClassAnnotation',
	#category : 'Commander-Calypso-Compatibility',
	#package : 'Commander-Calypso-Compatibility'
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

{ #category : 'queries-general' }
ClyCommandActivationStrategy class >> registeredInstances [
	"My class annotations and the commands declared for my interaction with the <calypsoCommands> pragma (see ClyCommandRegistry), in the order of my registry container, which sorts them by priority. Every way Calypso enumerates activations goes through me, so it shows both while commands move from class annotations to declarations."

	| annotations entries |
	annotations := super registeredInstances.
	entries := self interactionName
		           ifNil: [ #(  ) ]
		           ifNotNil: [ :interactionName | ClyCommandRegistry default allEntriesOf: interactionName ].
	entries ifEmpty: [ ^ annotations ].
	^ annotations asArray , entries sort: self createContainerForRegistry sortBlock
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
