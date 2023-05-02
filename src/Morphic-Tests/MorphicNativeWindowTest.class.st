Class {
	#name : #MorphicNativeWindowTest,
	#superclass : #AbstractWindowManagerTest,
	#instVars : [
		'window'
	],
	#category : #'Morphic-Tests'
}

{ #category : #running }
MorphicNativeWindowTest >> setUp [

	super setUp.

	window := manager newWindowWithRoot: Morph new
]

{ #category : #tests }
MorphicNativeWindowTest >> testMaximizeWindow [

	window open.
	window maximize.
	self assert: window isMaximized equals: true.
	window restore
]

{ #category : #tests }
MorphicNativeWindowTest >> testMinimizeWindow [

	window open.

	window minimize.

	self assert: window isMinimized.

	window restore
]

{ #category : #tests }
MorphicNativeWindowTest >> testNewWindowDefaultsIsNotMaximize [

	window open.

	self assert: window isMaximized equals: false
]

{ #category : #tests }
MorphicNativeWindowTest >> testNewWindowDefaultsIsNotMinimized [

	window open.

	self assert: window isNotMinimized
]

{ #category : #tests }
MorphicNativeWindowTest >> testRestoreWindow [

	window open.
	window restore.

	self assert: window isNotRestored equals: false
]
