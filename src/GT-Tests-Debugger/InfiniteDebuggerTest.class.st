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
	
	process := [ TestClassForTestingDebuggerTest debug: #bla ] forkAt: 41.

	1000 milliSecond asDelay wait.
	self currentWorld doOneCycle.
	preWindow := (self currentWorld allMorphs
		select: [ :e | (e isKindOf: SystemWindow) and: [ e label beginsWith: 'Instance of ' ] ])
		first.
	debugButton := (preWindow allMorphs
		select: [ :e | (e isKindOf: PluggableButtonMorph) and: [ e label = 'Debug' ] ]) first.
	debugButton performAction.
	preWindow := nil.
	debugButton := nil.
	100 milliSecond asDelay wait.
	self currentWorld doOneCycle.
	debugWindow := (self currentWorld allMorphs
		select: [ :e | (e isKindOf: SystemWindow) and: [ e label beginsWith: 'Instance of Test' ] ])
		first.
	overButton := (debugWindow allMorphs
		select: [ :e | (e isKindOf: GLMPluggableButtonMorph) and: [ e label = 'Over' ] ]) first.

	overButton performAction.
	100 milliSecond asDelay wait.
	self currentWorld doOneCycle.

	process terminate.	
	debugWindow close.

]
