Class {
	#name : 'BaselineOfHeuristicCompletion',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfHeuristicCompletion',
	#package : 'BaselineOfHeuristicCompletion'
}

{ #category : 'baselines' }
BaselineOfHeuristicCompletion >> baseline: spec [
	<baseline>
	
	spec for: #common do: [ 
		spec package: 'HeuristicCompletion-Model'.
		spec package: 'HeuristicCompletion-Morphic' with: [ 
			spec requires: 'HeuristicCompletion-Model' ].
		spec package: 'HeuristicCompletion-Tests' with: [ 
			spec requires: 'HeuristicCompletion-Model' ].
		spec package: 'HeuristicCompletion-Benchmarks' with:[
			spec requires: 'HeuristicCompletion-Model']
		].
	

]
