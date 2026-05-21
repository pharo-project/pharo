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
		spec baseline: 'AIEditDistances' with: [ spec repository: 'github://pharo-ai/edit-distances:main' ].
		
		spec package: 'HeuristicCompletion-Model' with: [
            spec requires: #('AIEditDistances') ].
			
		spec package: 'HeuristicCompletion-Analyser-Data'.
		spec package: 'HeuristicCompletion-Analyser-Data2'.
		spec package: 'HeuristicCompletion-Analyser-Data3'.
		spec package: 'HeuristicCompletion-Analyser-Data4'.
		spec package: 'HeuristicCompletion-Morphic' with: [ 
			spec requires: #('HeuristicCompletion-Model') ].
		spec package: 'HeuristicCompletion-Tests' with: [ 
			spec requires: #( 'HeuristicCompletion-Model' 
			'HeuristicCompletion-Analyser-Data' 
			'HeuristicCompletion-Analyser-Data2' 
			'HeuristicCompletion-Analyser-Data3'  
			'HeuristicCompletion-Analyser-Data4' ) ].
		spec package: 'HeuristicCompletion-Benchmarks' with:[
			spec requires: #('HeuristicCompletion-Model')
		].
		spec package: 'HeuristicCompletion-Benchmarks-Tests' with: [ 
			spec requires: #('HeuristicCompletion-Benchmarks') 
		].
	
		spec group: 'default' with: #(
			'HeuristicCompletion-Model'
		).
	
	].
]
