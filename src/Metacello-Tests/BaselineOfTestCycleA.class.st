Class {
	#name : 'BaselineOfTestCycleA',
	#superclass : 'BaselineOf',
	#category : 'Metacello-Tests',
	#package : 'Metacello-Tests'
}

{ #category : 'baselines' }
BaselineOfTestCycleA >> baseline: spec [
	<baseline>
	spec for: #common do: [
			spec package: 'CycleA'.
			spec baseline: 'TestCycleB' with: [ spec repository: 'http://repo.com' ] ]
]
