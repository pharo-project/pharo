"
I run each FastTable test in a fresh world. Tables ask the current world for its display scale when they build their scroll bars, and a test must not use the running world.
"
Class {
	#name : 'FTTestCase',
	#superclass : 'TestCase',
	#category : 'Morphic-Widgets-FastTable-Tests',
	#package : 'Morphic-Widgets-FastTable-Tests'
}

{ #category : 'running' }
FTTestCase >> runCase [
	"Run in a fresh world, and give back the previous one even when the test fails"

	| priorWorld |
	priorWorld := ActiveWorld.
	ActiveWorld := WorldMorph new.
	[ super runCase ] ensure: [ ActiveWorld := priorWorld ]
]
