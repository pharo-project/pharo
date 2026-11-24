"
I check Morphic's initialization on a fresh desktop world, made active only during each test. I put the desktop logo back where it was.
"
Class {
	#name : 'MorphicInitializationTest',
	#superclass : 'TestCase',
	#instVars : [
		'world',
		'previousLogo'
	],
	#category : 'Morphic-Desktop-Tests',
	#package : 'Morphic-Desktop-Tests'
}

{ #category : 'running' }
MorphicInitializationTest >> setUp [

	super setUp.
	previousLogo := PolymorphSystemSettings desktopLogo.
	world := DesktopWorldMorph new.
	world extent: 1024 @ 768
]

{ #category : 'running' }
MorphicInitializationTest >> tearDown [
	"The desktop logo is one for the whole system: give it back to the world that had it"

	PolymorphSystemSettings setDesktopLogoWith: previousLogo.
	super tearDown
]

{ #category : 'tests' }
MorphicInitializationTest >> testTheDesktopLogoGoesBelowTheMenubar [

	| menubar logo |
	world becomeActiveDuring: [
		MorphicInitialization initializeWorld.
		logo := PolymorphSystemSettings desktopLogo ].

	menubar := world menubar.
	self assert: logo owner identicalTo: world.
	self assert: logo top >= menubar bottom
]
