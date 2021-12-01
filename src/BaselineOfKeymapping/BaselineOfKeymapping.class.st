Class {
	#name : #BaselineOfKeymapping,
	#superclass : #BaselineOf,
	#category : #BaselineOfKeymapping
}

{ #category : #baselines }
BaselineOfKeymapping >> baseline: spec [
	<baseline>
	
	spec for: #common do: [ 
		spec 
			package: 'Keymapping-Core';
			package: 'Keymapping-KeyCombinations';
			package: 'Keymapping-Pragmas';
			package: 'Keymapping-Settings';
			package: 'Keymapping-Tools-Spec';
			package: 'Keymapping-Tests'.
		
		spec 
			group: 'default' with: #('core' 'ui' 'tests'); 
			group: 'core' with: #(
				'Keymapping-Core' 
				'Keymapping-KeyCombinations' 
				'Keymapping-Pragmas');
			group: 'ui' with: #(
				'core' 
				'Keymapping-Settings'
				'Keymapping-Tools-Spec');
			group: 'tests' with: #(
				'core' 
				'Keymapping-Tests') ]
]

{ #category : #accessing }
BaselineOfKeymapping >> project [
	| aProject |
	
	aProject := super project.
	aProject loadType: #atomic.

	^ aProject
]
