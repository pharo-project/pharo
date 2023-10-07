Class {
	#name : 'BaselineOfKeymapping',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfKeymapping',
	#package : 'BaselineOfKeymapping'
}

{ #category : 'baselines' }
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
			group: 'core' with: #('Keymapping-Core' 'Keymapping-KeyCombinations' );
			group: 'morphic' with: #('core' 'Keymapping-Pragmas' 'Keymapping-Settings');
			group: 'spec' with: #('core' 'Keymapping-Tools-Spec');
			group: 'ui' with: #('core' 'morphic' 'spec');
			group: 'tests' with: #(
				'core' 
				'Keymapping-Tests') ]
]

{ #category : 'accessing' }
BaselineOfKeymapping >> project [
	| aProject |
	
	aProject := super project.
	aProject loadType: #atomic.

	^ aProject
]
