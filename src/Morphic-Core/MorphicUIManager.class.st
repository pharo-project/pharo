"
I am the Morphic UI manager. I own the Morphic UI process.
Dialogs, menus and other widget services are added by Morphic-Widgets; themed dialogs are delegated to MorphicWidgetFactory.
"
Class {
	#name : 'MorphicUIManager',
	#superclass : 'UIManager',
	#instVars : [
		'activeTranscript'
	],
	#classVars : [
		'UIProcess'
	],
	#category : 'Morphic-Core-Support',
	#package : 'Morphic-Core',
	#tag : 'Support'
}

{ #category : 'accessing' }
MorphicUIManager class >> isActiveManager [
	"Answer whether I should act as the active ui manager"
	^ true
]

{ #category : 'testing' }
MorphicUIManager class >> isValidForCurrentSystemConfiguration [

	^ Smalltalk isInteractive
]

{ #category : 'cleanup' }
MorphicUIManager class >> restartMethods [
   UIProcess ifNotNil: [
	| process |
	process := UIProcess.
	self new spawnNewProcess.
	process terminate. ]
]

{ #category : 'ui process' }
MorphicUIManager class >> uiProcess [

	^ UIProcess
]

{ #category : 'ui process' }
MorphicUIManager >> defer: aValuable [
	"aValuable will be executed in the next UI rendering cycle"
	self currentWorld defer: aValuable
]

{ #category : 'ui process' }
MorphicUIManager >> resumeUIProcess: aProcess [
	"Adopt aProcess as the project process -- probably because of proceeding from a debugger"

	UIProcess := aProcess.
	UIProcess resume
]

{ #category : 'ui process' }
MorphicUIManager >> spawnNewProcess [

	UIProcess := [ MorphicRenderLoop new doOneCycleWhile: [ true ] ] newProcess priority:
		             Processor userSchedulingPriority.
	UIProcess
		name: 'Morphic UI Process';
		resume
]

{ #category : 'ui process' }
MorphicUIManager >> terminateUIProcess [
	UIProcess suspend; terminate.
	UIProcess := nil "?"
]

{ #category : 'ui process' }
MorphicUIManager >> uiProcess [
	" Answer the currently active UI process for morphic world.
	Client should check explicitly if #uiProcess answers nil or not (see other implementations)"

	^ UIProcess
]

{ #category : 'global state' }
MorphicUIManager >> world [
	^ self currentWorld
]
