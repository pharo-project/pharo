Class {
	#name : 'BaselineOfNewValueHolder',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfNewValueHolder',
	#package : 'BaselineOfNewValueHolder'
}

{ #category : 'baselines' }
BaselineOfNewValueHolder >> baseline: spec [
	<baseline>
	
	spec for: #common do: [ 
		spec package: 'NewValueHolder-Core'.
		spec package: 'NewValueHolder-Tests' 
			with: [ spec requires: #('NewValueHolder-Core') ].
		
		spec group: 'default' with: #('core' 'tests').
		spec group: 'core' with: #('NewValueHolder-Core').
		spec group: 'tests' with: #('NewValueHolder-Tests') ]
]

{ #category : 'accessing' }
BaselineOfNewValueHolder >> project [
	
	| aProject |
	
	aProject := super project.
	aProject loadType: #atomic.
	^ aProject
]
