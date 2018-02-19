Class {
	#name : #GLMPagerMorphTest,
	#superclass : #TestCase,
	#instVars : [
		'pagerModel',
		'pagerMorph',
		'window'
	],
	#category : #'Glamour-Tests-Morphic'
}

{ #category : #accessing }
GLMPagerMorphTest >> buttonsMorphs [

	^ pagerMorph subbricks second subbricks second subbricks
]

{ #category : #private }
GLMPagerMorphTest >> createSimplePaneMorph [

	^ Morph new
]

{ #category : #private }
GLMPagerMorphTest >> openInWindow [

	window := pagerMorph asBrickedMorph openInWindow 
]

{ #category : #accessing }
GLMPagerMorphTest >> panesMorphs [

	^ pagerMorph subbricks first subbricks first subbricks
]

{ #category : #running }
GLMPagerMorphTest >> setUp [
	super setUp.
	pagerModel := GLMPagerModel new.
	pagerMorph := GLMPagerBrick new
		pagerModel: pagerModel
		yourself.
]

{ #category : #running }
GLMPagerMorphTest >> tearDown [

	window ifNotNil: [ window delete ].
	super tearDown
]

{ #category : #tests }
GLMPagerMorphTest >> testPushPanesAfterOpen [

	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	self openInWindow.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	
	self assert: pagerModel size equals: 3.
	self assert: self panesMorphs size equals: 3.
	self assert: self buttonsMorphs size equals: 3.
	
	

]

{ #category : #tests }
GLMPagerMorphTest >> testPushPanesBeforeOpen [

	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.

	self openInWindow.
	
	self assert: pagerModel size equals: 3.
	self assert: self panesMorphs size equals: 3.
	self assert: self buttonsMorphs size equals: 3.
	
	

]
