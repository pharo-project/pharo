Class {
	#name : 'PragmaCollectorTest',
	#superclass : 'TestCase',
	#instVars : [
		'collector'
	],
	#category : 'PragmaCollector-Tests',
	#package : 'PragmaCollector-Tests'
}

{ #category : 'running' }
PragmaCollectorTest >> setUp [

	super setUp.
	collector := PragmaCollector new
]

{ #category : 'tests' }
PragmaCollectorTest >> testInstanceCreation [

	self assert: collector announcer notNil.
	self assert: collector isEmpty
]

{ #category : 'tests' }
PragmaCollectorTest >> testRelease [

	collector release.
	self assert: collector announcer isNil.
	self assert: collector isEmpty
]
