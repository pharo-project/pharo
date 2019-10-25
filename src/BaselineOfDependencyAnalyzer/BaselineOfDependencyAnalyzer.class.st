Class {
	#name : #BaselineOfDependencyAnalyzer,
	#superclass : #BaselineOf,
	#category : #BaselineOfDependencyAnalyzer
}

{ #category : #baselines }
BaselineOfDependencyAnalyzer >> baseline: spec [
	<baseline>
	
	spec for: #'common' do: [
			
		spec 
			package: 'Moose-Algos-Graph';
			package: 'Tool-DependencyAnalyser';
			package: 'Tool-DependencyAnalyser-UI';
			package: 'Moose-Algos-Graph-Tests';
			package: 'Tool-DependencyAnalyser-Tests';
			package: 'Tool-DependencyAnalyser-Tests-Data'.
		spec 
			group: 'Core' with: #('Moose-Algos-Graph' 	'Tool-DependencyAnalyser'
				'Tool-DependencyAnalyser-UI');
			group: 'Tests' with: #('Moose-Algos-Graph-Tests' 	'Tool-DependencyAnalyser-Tests'
				'Tool-DependencyAnalyser-Tests-Data');
			group: 'default' with: #('Core' 'Tests') ]
]
