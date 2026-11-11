"
I am the keyboard dispatcher of the Calypso tools. I am attached to a morph and I run the Calypso shortcuts visible in the context of the command provider before the default keymapping of the morph gets the key event.
"
Class {
	#name : 'ClyKMDispatcher',
	#superclass : 'KMDispatcher',
	#instVars : [
		'commandProvider'
	],
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'instance creation' }
ClyKMDispatcher class >> attachedTo: aMorph [
	| dispatcher |
	aMorph valueOfProperty: #kmDispatcher ifPresentDo: [ :existing | ^ existing ].
	dispatcher := self target: aMorph.
	aMorph initializeShortcuts: dispatcher.
	aMorph setProperty: #kmDispatcher toValue: dispatcher.
	^ dispatcher
]

{ #category : 'instance creation' }
ClyKMDispatcher class >> attachedTo: aMorph withCommandsFrom: someObjectWithCommandContext [

	| dispatcher |
	dispatcher := self attachedTo: aMorph.
	dispatcher commandProvider: someObjectWithCommandContext.
	^dispatcher
]

{ #category : 'dispatching' }
ClyKMDispatcher >> commandProvider [
	^commandProvider ifNil: [target morph]
]

{ #category : 'accessing' }
ClyKMDispatcher >> commandProvider: someObjectWithCommandContext [
	commandProvider := someObjectWithCommandContext
]

{ #category : 'dispatching' }
ClyKMDispatcher >> dispatch: anEventBuffer [

	| context executed |
	context := self commandProvider createCommandContext.

	ClyShortcutActivation visibleInstancesInContext: context do: [ :shortcut |
		executed := shortcut tryExecuteCommandInContext: context byEvents: anEventBuffer.
		executed ifTrue: [ ^self ] ].

	super dispatch: anEventBuffer
]
