Class {
	#name : #SindarinDebugSessionTest,
	#superclass : #TestCase,
	#instVars : [
		'debugSession',
		'sindarinSession'
	],
	#category : #'Sindarin-Tests-Base'
}

{ #category : #running }
SindarinDebugSessionTest >> setUp [
	"Hooks that subclasses may override to define the fixture of test."
	
	super setUp.
	debugSession := DebugSession new.
	sindarinSession := debugSession asSindarinDebugSession
]

{ #category : #tests }
SindarinDebugSessionTest >> testDebugSessionAsSindarinDebugSession [

	self assert: sindarinSession debugSession identicalTo: debugSession
]

{ #category : #tests }
SindarinDebugSessionTest >> testSindarinSessionAsSindarinDebugSession [

	self
		assert: sindarinSession asSindarinDebugSession
		identicalTo: sindarinSession
]

{ #category : #tests }
SindarinDebugSessionTest >> testSindarinSessionInstantiation [

	| sessionName process |
	sessionName := 'TestSDS'.
	process := [  ] newProcess.
	sindarinSession := SindarinDebugSession
		                   newWithName: sessionName
		                   forProcess: process.
	self assert: sindarinSession debugSession notNil.
	self assert: sindarinSession debugSession name equals: sessionName.
	self
		assert: sindarinSession debugSession process
		identicalTo: process
]
