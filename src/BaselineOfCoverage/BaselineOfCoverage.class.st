Class {
	#name : #BaselineOfCoverage,
	#superclass : #BaselineOf,
	#category : #BaselineOfCoverage
}

{ #category : #baselines }
BaselineOfCoverage >> baseline: spec [
	<baseline>
	spec
		for: #common
		do: [ 
			spec
				package: 'Coverage' ]
]
