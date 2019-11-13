Class {
	#name : #BaselineOfManifest,
	#superclass : #BaselineOf,
	#category : #BaselineOfManifest
}

{ #category : #baselines }
BaselineOfManifest >> baseline: spec [
	<baseline>
	
	spec for: #'common' do: [
			
		spec 
			package: 'Manifest-Core';
			package: 'Manifest-Resources-Tests';
			package: 'Manifest-Tests'.
		spec 
			group: 'Core' with: #('Manifest-Core');
			group: 'Tests' with: #('Manifest-Resources-Tests' 'Manifest-Tests');
			group: 'default' with: #('Core' 'Tests') ]
]
