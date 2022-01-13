Class {
	#name : #BaselineOfNewValueHolder,
	#superclass : #BaselineOf,
	#category : #BaselineOfNewValueHolder
}

{ #category : #baselines }
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
