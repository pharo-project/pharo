Class {
	#name : #BaselineOfKeymapping,
	#superclass : #BaselineOf,
	#category : #BaselineOfKeymapping
}

{ #category : #baselines }
BaselineOfKeymapping >> baseline: spec [
	<baseline>
	
	spec for: #common with: [ 
		spec 
			package: 'Keymapping-Core';
			package: 'Keymapping-KeyCombinations';
			package: 'Keymapping-Pragmas';
			package: 'Keymapping-Settings';
			package: 'Keymapping-Tools-Spec';
			package: 'Keymapping-Tests'.
		
		spec 
			group: 'default' with: #('core' 'ui' 'tests'); 
			group: 'core' with: #('Keymapping-Core');
			group: 'ui' with: #('core' 'Keymapping-KeyCombinations' 'Keymapping-Pragmas');
			group: 'tests' with: #('core' 'Keymapping-Tests') ]

]
