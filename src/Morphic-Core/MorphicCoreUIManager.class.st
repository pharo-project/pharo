"
This is a very simplified version of MorphicUIManager for Moprhic subset that contains ony Morphic-Core package and has only support of the most basic morphs.
"
Class {
	#name : #MorphicCoreUIManager,
	#superclass : #DummyUIManager,
	#classVars : [
		'UIProcess'
	],
	#category : #'Morphic-Core-Support'
}

{ #category : #'ui requests' }
MorphicCoreUIManager >> currentWorld [

	ActiveWorld ifNotNil: [ ^ ActiveWorld ].
	^ World
]

{ #category : #'ui requests' }
MorphicCoreUIManager >> newDisplayDepthNoRestore: pixelSize [

  "Change depths.  Check if there is enough space!  , di"
  | area need |
  pixelSize = Display depth ifTrue: [^ Display  "no change"].
  pixelSize abs < Display depth ifFalse:
    ["Make sure there is enough space"
    area := Display boundingBox area. "pixels"
    need := (area * (pixelSize abs - Display depth) // 8)  "new bytes needed"
        + Smalltalk lowSpaceThreshold.
    (Smalltalk garbageCollectMost <= need
      and: [Smalltalk garbageCollect <= need])
      ifTrue: [self error: 'Insufficient free space']].
  Display setExtent: Display extent depth: pixelSize.

  DisplayScreen startUp
]

{ #category : #'ui requests' }
MorphicCoreUIManager >> restoreDisplayAfter: aBlock [

  aBlock value.
  self currentWorld activeHand waitButton.
  self currentWorld fullRepaintNeeded.
]

{ #category : #'ui process' }
MorphicCoreUIManager >> resumeUIProcess: aProcess [
  "Adopt aProcess as the project process -- probably because of proceeding from a debugger"
      
  UIProcess := aProcess.
  UIProcess resume
]

{ #category : #'ui process' }
MorphicCoreUIManager >> spawnNewProcess [

  UIProcess := [
    [WorldMorph doOneCycle.  Processor yield.  false] whileFalse: [].
  ] newProcess priority: Processor userSchedulingPriority.
  UIProcess name: 'Morphic UI Process'.
  UIProcess resume
]

{ #category : #'ui process' }
MorphicCoreUIManager >> terminateUIProcess [
  UIProcess suspend; terminate.
  UIProcess := nil "?"
]

{ #category : #'ui process' }
MorphicCoreUIManager >> uiProcess [
  " Answer the currently active UI process for morphic world.
  Client should check explicitly if #uiProcess answers nil or not (see other implementations)"

  ^ UIProcess
]
