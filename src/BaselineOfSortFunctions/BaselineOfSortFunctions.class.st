Class {
	#name : #BaselineOfSortFunctions,
	#superclass : #BaselineOf,
	#category : #BaselineOfSortFunctions
}

{ #category : #baselines }
BaselineOfSortFunctions >> baseline: spec [
	<baseline>
	spec for: #'common' do: [
		spec 
			package: 'SortFunctions-Core';
			package: 'SortFunctions-Tests'.
		spec 
			group: 'Core' with: #('SortFunctions-Core');
			group: 'Tests' with: #('SortFunctions-Tests');
			group: 'default' with: #('Core' 'Tests') ]
]
