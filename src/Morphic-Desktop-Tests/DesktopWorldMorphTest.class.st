"
I check that a desktop world created programmatically is a desktop: it has the desktop's root event handlers.
"
Class {
	#name : 'DesktopWorldMorphTest',
	#superclass : 'TestCase',
	#category : 'Morphic-Desktop-Tests',
	#package : 'Morphic-Desktop-Tests'
}

{ #category : 'tests' }
DesktopWorldMorphTest >> testAPlainWorldIsNoDesktop [

	self assertEmpty: WorldMorph new rootEventHandlers
]

{ #category : 'tests' }
DesktopWorldMorphTest >> testHasTheDesktopHandlers [

	self assert: (DesktopWorldMorph new rootEventHandlers collect: [ :handler | handler class ]) asArray equals: {
		DesktopRootEventHandler.
		TaskbarRootEventHandler.
		HaloRootEventHandler }
]
