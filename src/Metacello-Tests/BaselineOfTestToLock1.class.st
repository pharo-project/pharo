"
A baseline to test lock/unlock
"
Class {
	#name : 'BaselineOfTestToLock1',
	#superclass : 'BaselineOf',
	#category : 'Metacello-Tests-Specs',
	#package : 'Metacello-Tests',
	#tag : 'Specs'
}

{ #category : 'testing' }
BaselineOfTestToLock1 class >> isProject [

	^ false
]

{ #category : 'baselines' }
BaselineOfTestToLock1 >> baseline: spec [
    <baseline>

    spec for: #common do: [
		spec package: 'Package1'.
		spec baseline: 'TestToLock2' ]
]
