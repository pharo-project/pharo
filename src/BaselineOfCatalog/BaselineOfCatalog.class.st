Class {
	#name : #BaselineOfCatalog,
	#superclass : #BaselineOf,
	#category : #BaselineOfCatalog
}

{ #category : #baselines }
BaselineOfCatalog >> baseline: spec [
	<baseline>
	spec for: #'common' do: [
		spec 
			package: 'Tool-Catalog' ]
]
