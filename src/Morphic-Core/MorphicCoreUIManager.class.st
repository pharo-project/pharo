"
This is a very simplified version of MorphicUIManager for Moprhic subset that contains ony Morphic-Core package and has only support of the most basic morphs.
"
Class {
	#name : 'MorphicCoreUIManager',
	#superclass : 'DummyUIManager',
	#category : 'Morphic-Core-Support',
	#package : 'Morphic-Core',
	#tag : 'Support'
}

{ #category : 'cleanup' }
MorphicCoreUIManager class >> restartMethods [

	Process uiProcess ifNotNil: [
		| process |
		process := Process uiProcess.
		self new spawnNewProcess.
		process terminate ]
]

{ #category : 'ui requests' }
MorphicCoreUIManager >> currentWorld [

	ActiveWorld ifNotNil: [ ^ ActiveWorld ].
	^ World
]

{ #category : 'ui requests' }
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

{ #category : 'ui requests' }
MorphicCoreUIManager >> restoreDisplayAfter: aBlock [

  aBlock value.
  self currentWorld activeHand waitButton.
  self currentWorld fullRepaintNeeded
]

{ #category : 'ui process' }
MorphicCoreUIManager >> spawnNewProcess [

	Process
		spawnNewUIProcess:
			([ MorphicRenderLoop new doOneCycleWhile: [ true ] ] newProcess
				 priority: Processor userSchedulingPriority)
		named: 'Morphic UI Process'
]

{ #category : 'ui process' }
MorphicCoreUIManager >> terminateUIProcess [

	Process terminateUIProcess
]

{ #category : 'ui process' }
MorphicCoreUIManager >> uiProcess [
  " Answer the currently active UI process for morphic world.
  Client should check explicitly if #uiProcess answers nil or not (see other implementations)"

  ^ Process uiProcess
]
