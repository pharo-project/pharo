Class {
	#name : #InfiniteDebuggerTest,
	#superclass : #TestCase,
	#category : #'GT-Tests-Debugger'
}

{ #category : #running }
InfiniteDebuggerTest >> runCaseManaged [
	
	self runCase
]

{ #category : #tests }
InfiniteDebuggerTest >> testTestInfinitDebugger [
	| preWindow debugButton debugWindow overButton process |
	<expectedFailure>
	
	process := [ TestClassForTestingDebugger debug: #bla ] forkAt: 41.

	1000 milliSecond asDelay wait.
	World doOneCycle.
	preWindow := (World allMorphs
		select: [ :e | (e isKindOf: SystemWindow) and: [ e label beginsWith: 'Instance of ' ] ])
		first.
	debugButton := (preWindow allMorphs
		select: [ :e | (e isKindOf: PluggableButtonMorph) and: [ e label = 'Debug' ] ]) first.
	debugButton performAction.
	preWindow := nil.
	debugButton := nil.
	100 milliSecond asDelay wait.
	World doOneCycle.
	debugWindow := (World allMorphs
		select: [ :e | (e isKindOf: SystemWindow) and: [ e label beginsWith: 'Instance of Test' ] ])
		first.
	overButton := (debugWindow allMorphs
		select: [ :e | (e isKindOf: GLMPluggableButtonMorph) and: [ e label = 'Over' ] ]) first.

	overButton performAction.
	100 milliSecond asDelay wait.
	World doOneCycle.

	process terminate.	
	debugWindow close.

]
