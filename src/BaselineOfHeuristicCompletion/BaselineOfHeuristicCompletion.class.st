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
		spec baseline: 'AIEditDistances' with: [ spec repository: 'github://pharo-ai/edit-distances:main/src' ].
		
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
	
	"Groups"
		spec group: 'core' with: #(
    		'HeuristicCompletion-Model'
    		'HeuristicCompletion-Morphic'
		).

		spec group: 'data' with: #(
    		'HeuristicCompletion-Analyser-Data'
    		'HeuristicCompletion-Analyser-Data2'
    		'HeuristicCompletion-Analyser-Data3'
    		'HeuristicCompletion-Analyser-Data4'
		).

		spec group: 'tests' with: #(
    		'HeuristicCompletion-Tests'
    		'HeuristicCompletion-Benchmarks-Tests'
		).

		spec group: 'benchmarks' with: #(
    		'HeuristicCompletion-Benchmarks'
		).

		spec group: 'default' with: #(
    		'core'
    		'data'
    		'tests'
    		'benchmarks'
		).
	
	].
]
