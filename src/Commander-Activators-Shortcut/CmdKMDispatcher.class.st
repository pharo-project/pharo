"
I implement dispatch by delegating execution to appropriate command which defines shortcut activation strategy suitable for given KM events.

User should override morph kmDispatcher to use me instead of default:

	YourMorph>>kmDispatcher
		^ CmdKMDispatcher attachedTo: self
		
In case when commands should be provided by another object instead of morph you should use another method: 
	
	CmdKMDispatcher attachedTo: self withCommandsFrom: someObjectWithCommandContext
	
During dispatch process I ask command provider to create command context. And by default provider is morph itself.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	commandProvider:		<Object>
"
Class {
	#name : #CmdKMDispatcher,
	#superclass : #KMDispatcher,
	#instVars : [
		'commandProvider'
	],
	#category : #'Commander-Activators-Shortcut'
}

{ #category : #'instance creation' }
CmdKMDispatcher class >> attachedTo: aMorph [

	| dispatcher |
	aMorph valueOfProperty: #kmDispatcher ifPresentDo: [:existing | 
		(existing isKindOf: self) ifTrue: [^existing]].
	dispatcher := CmdKMDispatcher target: aMorph.
	aMorph initializeShortcuts: dispatcher.
	aMorph setProperty: #kmDispatcher toValue: dispatcher.
	^dispatcher
]

{ #category : #'instance creation' }
CmdKMDispatcher class >> attachedTo: aMorph withCommandsFrom: someObjectWithCommandContext [

	| dispatcher |
	dispatcher := self attachedTo: aMorph.
	dispatcher commandProvider: someObjectWithCommandContext.
	^dispatcher
]

{ #category : #dispatching }
CmdKMDispatcher >> commandProvider [
	^commandProvider ifNil: [target morph]
]

{ #category : #accessing }
CmdKMDispatcher >> commandProvider: someObjectWithCommandContext [
	commandProvider := someObjectWithCommandContext
]

{ #category : #dispatching }
CmdKMDispatcher >> dispatch: anEventBuffer [

	| context executed |
	context := self commandProvider createCommandContext.
	
	CmdShortcutActivation visibleInstancesInContext: context do: [ :shortcut | 
		executed := shortcut tryExecuteCommandInContext: context byEvents: anEventBuffer.
		executed ifTrue: [ ^self ] ].
	
	super dispatch: anEventBuffer 
]
