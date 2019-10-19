Class {
	#name : #BaselineOfReflectivity,
	#superclass : #BaselineOf,
	#category : #BaselineOfReflectivity
}

{ #category : #baselines }
BaselineOfReflectivity >> baseline: spec [
	<baseline>
	
	spec for: #'common' do: [
			
		spec 
			package: 'Reflectivity-Tests';
			package: 'Reflectivity-Examples' ]
]
