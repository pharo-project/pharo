"
I am a concrete visitor that reucrsively collects all the commands of a command registry. I'm used by the `CmCommandGroup>>#allCommands` message.
"
Class {
	#name : #CmCommandCollector,
	#superclass : #CmVisitor,
	#instVars : [
		'commandsCollected'
	],
	#category : #'Commander2-Visitors'
}

{ #category : #accessing }
CmCommandCollector >> commandsCollected [
	^ commandsCollected
]

{ #category : #initialization }
CmCommandCollector >> initialize [
	super initialize.
	commandsCollected := OrderedCollection new
]

{ #category : #visiting }
CmCommandCollector >> visitCommand: aCmCommand [
	commandsCollected add: aCmCommand.
	^ aCmCommand
]
