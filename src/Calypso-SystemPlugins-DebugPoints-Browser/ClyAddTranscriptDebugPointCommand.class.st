"
I define a command to add a breakpoint with a Transcript behavior on the AST node corresponding to the selected code
"
Class {
	#name : 'ClyAddTranscriptDebugPointCommand',
	#superclass : 'ClyDebuggingPointsCommand',
	#category : 'Calypso-SystemPlugins-DebugPoints-Browser',
	#package : 'Calypso-SystemPlugins-DebugPoints-Browser'
}

{ #category : 'testing' }
ClyAddTranscriptDebugPointCommand class >> canBeExecutedInContext: aBrowserContext [

	^ (super canBeExecutedInContext: aBrowserContext)
]

{ #category : 'activation' }
ClyAddTranscriptDebugPointCommand class >> contextMenuOrder [
	^4
]

{ #category : 'accessing' }
ClyAddTranscriptDebugPointCommand >> defaultMenuIconName [
	^#transcript
]

{ #category : 'accessing' }
ClyAddTranscriptDebugPointCommand >> defaultMenuItemName [
	^' Add Transcript to: ',  sourceNode displaySourceCode
]

{ #category : 'execution' }
ClyAddTranscriptDebugPointCommand >> execute [

	| dp | 
	dp := (DebugPointManager
		       installNew: DebugPoint
		       on: sourceNode
		       withBehaviors: { TranscriptBehavior }) name: #Transcript.
	dp text: self popUp.
	self putIconInProperties: dp
]

{ #category : 'ui - dialogs' }
ClyAddTranscriptDebugPointCommand >> popUp [
	|textInput|
	textInput := SpRequestDialog new
		label: 'Enter string:';
		text: '';
		title: 'Transcript String';
		openModal.
	textInput ifNil: [ ^'' ].
	^textInput 
]
