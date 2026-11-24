"
I check the menubar when no world is installed yet, as while Pharo loads.
"
Class {
	#name : 'MenubarMorphTest',
	#superclass : 'TestCase',
	#category : 'Morphic-Tests-Widgets',
	#package : 'Morphic-Tests',
	#tag : 'Widgets'
}

{ #category : 'tests' }
MenubarMorphTest >> testResetWithoutAWorldDoesNothing [

	self assert: MenubarMorphWithoutWorld reset equals: MenubarMorphWithoutWorld
]
