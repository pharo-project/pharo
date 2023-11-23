"
A baseline to test lock/unlock (this one will be nested to `BaselineOfTestToLock1`
"
Class {
	#name : 'BaselineOfTestToLock2',
	#superclass : 'BaselineOf',
	#category : 'Metacello-TestsCore-Specs',
	#package : 'Metacello-TestsCore',
	#tag : 'Specs'
}

{ #category : 'testing' }
BaselineOfTestToLock2 class >> isProject [

	^ false
]

{ #category : 'baselines' }
BaselineOfTestToLock2 >> baseline: spec [
	<baseline>
	
	spec for: #common do: [ 
		spec package: 'TestToLockPackage1'.
		spec package: 'TestToLockPackage2' ]
]
