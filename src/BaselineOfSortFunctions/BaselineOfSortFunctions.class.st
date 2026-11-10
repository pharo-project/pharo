Class {
	#name : 'BaselineOfSortFunctions',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfSortFunctions',
	#package : 'BaselineOfSortFunctions'
}

{ #category : 'baselines' }
BaselineOfSortFunctions >> baseline: spec [
	<baseline>
	spec for: #'common' do: [
		spec 
			package: 'SortFunctions-Core';
			package: 'SortFunctions-Tests' with: [ spec requires: #( 'SortFunctions-Core') ].
		spec 
			group: 'Core' with: #('SortFunctions-Core');
			group: 'Tests' with: #('SortFunctions-Tests');
			group: 'default' with: #('Core' 'Tests') ]
]
