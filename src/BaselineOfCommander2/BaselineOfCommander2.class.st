Class {
	#name : #BaselineOfCommander2,
	#superclass : #BaselineOf,
	#category : #BaselineOfCommander2
}

{ #category : #baselines }
BaselineOfCommander2 >> baseline: spec [
	<baseline>
	
	spec for: #common do: [ 
		spec
			package: 'Commander2';
			package: 'Commander2-Tests' with: [ spec requires: #('Commander2') ] ]
]
