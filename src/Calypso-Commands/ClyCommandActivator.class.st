"
I am a Calypso activator: I bind a command to the context where it is executed and to the activation strategy which made it available. I execute the command with the Commander2 lifecycle, so the command prepares, executes and applies its own result. The context only provides the state and the failure notification.
"
Class {
	#name : 'ClyCommandActivator',
	#superclass : 'Object',
	#instVars : [
		'command',
		'context',
		'activationStrategy'
	],
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'instance creation' }
ClyCommandActivator class >> createCommand: aCommandClass inContext: aToolContext [
	^ aCommandClass new
		context: aToolContext;
		yourself
]

{ #category : 'instance creation' }
ClyCommandActivator class >> for: aCommandClass inContext: aToolContext using: aCommandActivationStrategy [
	| command |
	command := self createCommand: aCommandClass inContext: aToolContext.

	^self new
		command: command;
		context: aToolContext;
		activationStrategy: aCommandActivationStrategy
]

{ #category : 'testing' }
ClyCommandActivator >> activatesCommand: aCommandClass [
	^activationStrategy annotates: aCommandClass
]

{ #category : 'accessing' }
ClyCommandActivator >> activationStrategy [
	^ activationStrategy
]

{ #category : 'accessing' }
ClyCommandActivator >> activationStrategy: anObject [
	activationStrategy := anObject
]

{ #category : 'converting' }
ClyCommandActivator >> asCommandActivator [
	^self
]

{ #category : 'testing' }
ClyCommandActivator >> canExecuteCommand [

	^activationStrategy isActiveInContext: context
]

{ #category : 'accessing' }
ClyCommandActivator >> command [
	^ command
]

{ #category : 'accessing' }
ClyCommandActivator >> command: anObject [
	command := anObject
]

{ #category : 'accessing' }
ClyCommandActivator >> commandDescription [

	^command description ifNil: [ self menuItemName ]
]

{ #category : 'accessing' }
ClyCommandActivator >> context [
	^ context
]

{ #category : 'accessing' }
ClyCommandActivator >> context: anObject [
	context := anObject
]

{ #category : 'executing' }
ClyCommandActivator >> executeCommand [
	[ command context: context.
		command execute ]
	on: Exception do: [ :exc | context processFailure: exc of: command ]
]

{ #category : 'spec2 compatibility' }
ClyCommandActivator >> help [
	^ self command class name
]

{ #category : 'testing' }
ClyCommandActivator >> isSimilarTo: anotherCommandActivator [

	^(activationStrategy isSimilarTo: anotherCommandActivator activationStrategy)
		and: [context isSimilarTo: anotherCommandActivator context ]
]

{ #category : 'accessing' }
ClyCommandActivator >> menuItemName [
	^activationStrategy menuItemNameFor: command
]

{ #category : 'accessing' }
ClyCommandActivator >> menuItemOrder [
	^activationStrategy menuItemOrder
]

{ #category : 'accessing' }
ClyCommandActivator >> newForCommand: aCommand [

	| copy |
	copy := self copy.
	copy command: aCommand.
	^copy
]

{ #category : 'printing' }
ClyCommandActivator >> printOn: aStream [
	super printOn: aStream.

	aStream nextPut: $(.
	command printOn: aStream.
	aStream nextPut: $)
]

{ #category : 'context menu' }
ClyCommandActivator >> setUpShortcutTipForMenuItem: aMenuItemMorph [

	ClyShortcutActivation
		activeInstancesFor: self command class inContext: self context
		do: [ :shortcut |
			aMenuItemMorph keyText: (shortcut keyCombination acceptVisitor: OSPlatform current shortcutPrinter)]
]
