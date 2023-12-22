"
This is a very simplified version of MorphicUIManager for Moprhic subset that contains ony Morphic-Core package and has only support of the most basic morphs.
"
Class {
	#name : 'MorphicCoreUIManager',
	#superclass : 'DummyUIManager',
	#classVars : [
		'UIProcess'
	],
	#category : 'Morphic-Core-Support',
	#package : 'Morphic-Core',
	#tag : 'Support'
}

{ #category : 'cleanup' }
MorphicCoreUIManager class >> restartMethods [
   UIProcess ifNotNil: [
	| process |
	process := UIProcess.
	self new spawnNewProcess.
	process terminate. ]
]

{ #category : 'ui requests' }
MorphicCoreUIManager >> currentWorld [

	ActiveWorld ifNotNil: [ ^ ActiveWorld ].
	^ World
]

{ #category : 'ui requests' }
MorphicCoreUIManager >> restoreDisplayAfter: aBlock [

  aBlock value.
  self currentWorld activeHand waitButton.
  self currentWorld fullRepaintNeeded
]

{ #category : 'ui process' }
MorphicCoreUIManager >> resumeUIProcess: aProcess [
  "Adopt aProcess as the project process -- probably because of proceeding from a debugger"

  UIProcess := aProcess.
  UIProcess resume
]

{ #category : 'ui process' }
MorphicCoreUIManager >> spawnNewProcess [

  UIProcess := [
	MorphicRenderLoop new doOneCycleWhile: [ true ]
  ] newProcess priority: Processor userSchedulingPriority.
  UIProcess name: 'Morphic UI Process'.
  UIProcess resume
]

{ #category : 'ui process' }
MorphicCoreUIManager >> terminateUIProcess [
  UIProcess suspend; terminate.
  UIProcess := nil "?"
]
