Class {
	#name : #MiniWorldTest,
	#superclass : #TestCase,
	#instVars : [
		'manager'
	],
	#category : #'Morphic-Tests'
}

{ #category : #running }
MiniWorldTest >> runCaseManaged [

	self runCase
]

{ #category : #running }
MiniWorldTest >> setUp [
	super setUp.
	manager := MorphicWindowManager new.
	
]

{ #category : #running }
MiniWorldTest >> tearDown [ 

	manager ensureDeleteAll.
	super tearDown

	
]

{ #category : #tests }
MiniWorldTest >> testAddMorph [

	| nativeWindow world morph |
	"This test do not pass on CI because headless image has display size = 0@0 and then 
	 morphic behaves weird. PropagateNaturalHeight/Width is weak on Morphic because of 
	 this (if not, see the algoritm I was forced to do), but for now passes (just, not on CI :)"
	Smalltalk isInteractiveGraphic ifFalse: [ ^ self skip ].
	nativeWindow := manager openNewWindowWithRoot: Morph new.
	world := nativeWindow window world.

	morph := Morph new.

	self assert: world submorphs size equals: 1.

	world addMorph: Morph new.
	world addMorph: morph.

	self assert: world submorphs last equals: morph.
	self assert: world submorphs size equals: 3
]

{ #category : #tests }
MiniWorldTest >> testAddMorphFront [

	| nativeWindow world morph |
	"This test do not pass on CI because headless image has display size = 0@0 and then 
	 morphic behaves weird. PropagateNaturalHeight/Width is weak on Morphic because of 
	 this (if not, see the algoritm I was forced to do), but for now passes (just, not on CI :)"
	Smalltalk isInteractiveGraphic ifFalse: [ ^ self skip ].
	nativeWindow := manager openNewWindowWithRoot:
		                ClyFullBrowserMorph onDefaultEnvironment.
	world := nativeWindow window world.
	world addMorph: Morph new.

	morph := Morph new.

	self assert: world submorphs size equals: 2.


	world addMorphFront: morph.

	self assert: world submorphs first equals: morph.
	self assert: world submorphs size equals: 3
]

{ #category : #tests }
MiniWorldTest >> testDeleteAWindowWithMiniWorldAndIsDeleteToExtraWorldList [

	| nativeWindow world |
	"This test do not pass on CI because headless image has display size = 0@0 and then 
	 morphic behaves weird. PropagateNaturalHeight/Width is weak on Morphic because of 
	 this (if not, see the algoritm I was forced to do), but for now passes (just, not on CI :)"
	Smalltalk isInteractiveGraphic ifFalse: [ ^ self skip ].

	nativeWindow := manager openNewWindowWithRoot:
		                ClyFullBrowserMorph onDefaultEnvironment.
	world := nativeWindow window world.

	self assert: world equals: WorldMorph extraWorldList last.

	nativeWindow delete.

	self deny: (WorldMorph extraWorldList includes: world)
]

{ #category : #tests }
MiniWorldTest >> testOpenAWindowWithMiniWorld [

	| nativeWindow world |
	"This test do not pass on CI because headless image has display size = 0@0 and then 
	 morphic behaves weird. PropagateNaturalHeight/Width is weak on Morphic because of 
	 this (if not, see the algoritm I was forced to do), but for now passes (just, not on CI :)"
	Smalltalk isInteractiveGraphic ifFalse: [ ^ self skip ].

	nativeWindow := manager openNewWindowWithRoot:
		                ClyFullBrowserMorph onDefaultEnvironment.
	world := nativeWindow window world.
	self assert: world isMiniWorld
]

{ #category : #tests }
MiniWorldTest >> testOpenAWindowWithMiniWorldAndIsAppendToExtraWorldList [

	| windo |
	"This test do not pass on CI because headless image has display size = 0@0 and then 
	 morphic behaves weird. PropagateNaturalHeight/Width is weak on Morphic because of 
	 this (if not, see the algoritm I was forced to do), but for now passes (just, not on CI :)"
	Smalltalk isInteractiveGraphic ifFalse: [ ^ self skip ].

	windo := manager openNewWindowWithRoot:
		         ClyFullBrowserMorph onDefaultEnvironment.

	self assert: windo backend equals: WorldMorph extraWorldList last
]
