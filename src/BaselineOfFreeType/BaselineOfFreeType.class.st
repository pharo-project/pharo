Class {
	#name : #BaselineOfFreeType,
	#superclass : #BaselineOf,
	#category : #BaselineOfFreeType
}

{ #category : #baselines }
BaselineOfFreeType >> baseline: spec [
	<baseline>
	
	spec for: #common do: [ 
		spec package: 'FreeType'.
		spec package: 'FreeType-Morphic'.
		spec package: 'FreeType-Tests'.
		spec package: 'FreeType-Help'.

		spec group: 'core' with: #('FreeType').
		spec group: 'morphic' with: #('core' 'FreeType-Morphic').
		spec group: 'test' with: #('core' 'FreeType-Tests') ]
]

{ #category : #accessing }
BaselineOfFreeType >> project [
	
	| aProject |
	
	aProject := super project.
	aProject loadType: #atomic.
	^ aProject
]
