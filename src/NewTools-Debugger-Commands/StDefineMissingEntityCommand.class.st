"
This command requests from a debugger to create a missing method (DNU or subclassResponsibility) or class.

This command delegates concrete behavior to its subclasses.
Each instance of this command has a collection of mutually exclusive subcommands (one for each of its subclasses). 
Each subcommand has the same debugger.

For a given debugging context (i.e., the context of the debugger's debug session), only one of the subcommand can be executed.
For instance, if we are in the context of a DNU, it makes only sense to propose to create the missing method (StCreateMissingMethodDebugCommand) and all other subcommands are inapplicable.
"
Class {
	#name : #StDefineMissingEntityCommand,
	#superclass : #StDebuggerCommand,
	#instVars : [
		'subCommands'
	],
	#category : #'NewTools-Debugger-Commands'
}

{ #category : #accessing }
StDefineMissingEntityCommand class >> defaultDescription [
    ^ 'Create the missing class or method in the user prompted class, and restart the debugger at the location where it can be edited.'
]

{ #category : #initialization }
StDefineMissingEntityCommand class >> defaultIconName [

	^ #debuggerDefineEntity
]

{ #category : #accessing }
StDefineMissingEntityCommand class >> defaultName [
	^ 'Create'
]

{ #category : #initialization }
StDefineMissingEntityCommand class >> defaultShortcut [
	^ nil
]

{ #category : #private }
StDefineMissingEntityCommand >> applicableCommand [
	^ subCommands
		detect: [ :subCommand | subCommand canBeExecuted ]
		ifNone: [ nil ]
]

{ #category : #testing }
StDefineMissingEntityCommand >> appliesTo: aDebugger [
	^ subCommands
		anySatisfy: [ :subCommand | subCommand appliesTo: aDebugger ]
]

{ #category : #accessing }
StDefineMissingEntityCommand >> context: aDebugger [
	super context: aDebugger.
	subCommands := self class allSubclasses
		collect: [ :subCommand | subCommand forContext: aDebugger ]
]

{ #category : #executing }
StDefineMissingEntityCommand >> execute [
	self applicableCommand ifNotNil: [ :cmd | cmd execute ]
]

{ #category : #initialization }
StDefineMissingEntityCommand >> initialize [
	super initialize.
	subCommands := OrderedCollection new
]

{ #category : #accessing }
StDefineMissingEntityCommand >> subCommands [
	^subCommands
]
