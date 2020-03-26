Class {
	#name : #BaselineOfHeuristicCompletion,
	#superclass : #BaselineOf,
	#category : #BaselineOfHeuristicCompletion
}

{ #category : #baselines }
BaselineOfHeuristicCompletion >> baseline: spec [
	<baseline>
	
	spec for: #common do: [ 
		spec package: 'HeuristicCompletion'.
		spec package: 'HeuristicCompletion-Tests' ]
]
