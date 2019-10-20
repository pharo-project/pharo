Class {
	#name : #BaselineOfReferenceFinder,
	#superclass : #BaselineOf,
	#category : #BaselineOfReferenceFinder
}

{ #category : #baselines }
BaselineOfReferenceFinder >> baseline: spec [
	<baseline>
	spec for: #'common' do: [
		spec 
			package: 'ReferenceFinder-Core'.
		spec 
			group: 'default' with: #('ReferenceFinder-Core') ]
]
