Class {
	#name : 'MorphicNativeWindowTest',
	#superclass : 'AbstractWindowManagerTest',
	#instVars : [
		'window'
	],
	#category : 'Morphic-Tests',
	#package : 'Morphic-Tests'
}

{ #category : 'running' }
MorphicNativeWindowTest >> setUp [

	super setUp.
	window := manager newWindowWithRoot: Morph new
]

{ #category : 'tests' }
MorphicNativeWindowTest >> testMaximizeWindow [
	"This test do not pass on CI because headless image has display size = 0@0 and then 
	 morphic behaves weird. PropagateNaturalHeight/Width is weak on Morphic because of 
	 this (if not, see the algoritm I was forced to do), but for now passes (just, not on CI :)"

	Smalltalk isInteractiveGraphic ifFalse: [ ^ self skip ].

	window open.
	window maximize.
	self assert: window isMaximized equals: true.
	window restore
]

{ #category : 'tests' }
MorphicNativeWindowTest >> testMinimizeWindow [
	"This test do not pass on CI because headless image has display size = 0@0 and then 
	 morphic behaves weird. PropagateNaturalHeight/Width is weak on Morphic because of 
	 this (if not, see the algoritm I was forced to do), but for now passes (just, not on CI :)"

	Smalltalk isInteractiveGraphic ifFalse: [ ^ self skip ].
	window open.

	window minimize.

	self assert: window isMinimized.

	window restore
]

{ #category : 'tests' }
MorphicNativeWindowTest >> testNewWindowDefaultsIsNotMaximize [
	"This test do not pass on CI because headless image has display size = 0@0 and then 
	 morphic behaves weird. PropagateNaturalHeight/Width is weak on Morphic because of 
	 this (if not, see the algoritm I was forced to do), but for now passes (just, not on CI :)"

	Smalltalk isInteractiveGraphic ifFalse: [ ^ self skip ].
	window open.

	self assert: window isMaximized equals: false
]

{ #category : 'tests' }
MorphicNativeWindowTest >> testNewWindowDefaultsIsNotMinimized [
	"This test do not pass on CI because headless image has display size = 0@0 and then 
	 morphic behaves weird. PropagateNaturalHeight/Width is weak on Morphic because of 
	 this (if not, see the algoritm I was forced to do), but for now passes (just, not on CI :)"

	Smalltalk isInteractiveGraphic ifFalse: [ ^ self skip ].
	window open.

	self assert: window isNotMinimized
]

{ #category : 'tests' }
MorphicNativeWindowTest >> testRestoreWindow [
	"This test do not pass on CI because headless image has display size = 0@0 and then 
	 morphic behaves weird. PropagateNaturalHeight/Width is weak on Morphic because of 
	 this (if not, see the algoritm I was forced to do), but for now passes (just, not on CI :)"

	Smalltalk isInteractiveGraphic ifFalse: [ ^ self skip ].
	window open.
	window restore.

	self assert: window isNotRestored equals: false
]
