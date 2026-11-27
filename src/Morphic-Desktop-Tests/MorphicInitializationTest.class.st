"
I check Morphic's initialization of a world given as an argument.
"
Class {
	#name : 'MorphicInitializationTest',
	#superclass : 'TestCase',
	#category : 'Morphic-Desktop-Tests',
	#package : 'Morphic-Desktop-Tests'
}

{ #category : 'tests' }
MorphicInitializationTest >> testTheDesktopLogoGoesBelowTheMenubar [

	| world logo |
	world := DesktopWorldMorph new.
	world extent: 1024 @ 768.

	logo := MorphicInitialization initializeWorld: world.

	self assert: logo owner identicalTo: world.
	self assert: logo top >= world menubar bottom
]
