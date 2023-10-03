Class {
	#name : 'BaselineOfCoverage',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfCoverage',
	#package : 'BaselineOfCoverage'
}

{ #category : 'baselines' }
BaselineOfCoverage >> baseline: spec [
	<baseline>
	spec
		for: #common
		do: [ 
			spec
				package: 'Coverage' ]
]
