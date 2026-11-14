Class {
	#name : 'BaselineOfTestCycleB',
	#superclass : 'BaselineOf',
	#category : 'Metacello-Tests',
	#package : 'Metacello-Tests'
}

{ #category : 'baselines' }
BaselineOfTestCycleB >> baseline: spec [
	<baseline>
	spec for: #common do: [
			spec package: 'CycleB'.
			spec baseline: 'TestCycleA' with: [ spec repository: 'http://repo.com' ] ]
]
