"
A GLMScrollListBrickTest is a test class for testing the behavior of GLMScrollListBrick
"
Class {
	#name : #GLMScrollListBrickTest,
	#superclass : #TestCase,
	#instVars : [
		'window'
	],
	#category : #'Glamour-Morphic-Brick-Tests'
}

{ #category : #running }
GLMScrollListBrickTest >> tearDown [
	window ifNotNil: [ :aWindow | aWindow delete ].
	super tearDown
]

{ #category : #tests }
GLMScrollListBrickTest >> testDifferentHeights [
	window := GLMScrollListBrick exampleDifferentHeights.
	self assert: window isSystemWindow
]

{ #category : #tests }
GLMScrollListBrickTest >> testEmptyList [
	window := GLMScrollListBrick exampleEmptyCollection.
	self assert: window isSystemWindow
]

{ #category : #tests }
GLMScrollListBrickTest >> testExample [
	window := GLMScrollListBrick example.
	self assert: window isSystemWindow
]

{ #category : #tests }
GLMScrollListBrickTest >> testInfinity [
	window := GLMScrollListBrick exampleInfinity.
	self assert: window isSystemWindow
]
