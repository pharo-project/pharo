"
I check that AthensWrapWorldMorph embeds any world, plain or desktop. The wrapper is its own root: nothing goes into the running world.
"
Class {
	#name : 'AthensWrapWorldMorphTest',
	#superclass : 'TestCase',
	#category : 'Athens-Morphic-Tests',
	#package : 'Athens-Morphic-Tests'
}

{ #category : 'tests' }
AthensWrapWorldMorphTest >> testEmbeddedWorldTakesTheWrapperSize [

	| world wrapper |
	world := WorldMorph new.
	wrapper := AthensWrapWorldMorph on: world.
	wrapper extent: 300 @ 200.
	self assert: world extent equals: 300 @ 200.
	world restoreMorphicDisplay.
	self assert: world extent equals: 300 @ 200
]

{ #category : 'tests' }
AthensWrapWorldMorphTest >> testEmbedsADesktop [

	| desktop wrapper |
	desktop := DesktopWorldMorph new.
	wrapper := AthensWrapWorldMorph on: desktop.
	self assert: wrapper embeddedWorld identicalTo: desktop.
	self assert: desktop owner identicalTo: wrapper.
	self assert: desktop hands size equals: 1
]

{ #category : 'tests' }
AthensWrapWorldMorphTest >> testEmbedsAPlainWorldByDefault [

	| wrapper |
	wrapper := AthensWrapWorldMorph new.
	self assert: wrapper embeddedWorld class identicalTo: WorldMorph.
	self assert: wrapper embeddedWorld owner identicalTo: wrapper
]

{ #category : 'tests' }
AthensWrapWorldMorphTest >> testReplacingTheWorldRemovesTheOldOne [

	| wrapper old |
	wrapper := AthensWrapWorldMorph new.
	old := wrapper embeddedWorld.
	wrapper embeddedWorld: WorldMorph new.
	self assert: old owner isNil.
	self deny: (wrapper submorphs includes: old)
]
