"
Baseline managing the MenuRegistration project for the Pharo bootstrap.

Load type: 
- linear
"
Class {
	#name : #BaselineOfMenuRegistration,
	#superclass : #BaselineOf,
	#category : #BaselineOfMenuRegistration
}

{ #category : #baseline }
BaselineOfMenuRegistration >> baseline: spec [ 
	<baseline>
	
	spec for: #common do: [
	
		spec package: 'MenuRegistration'.
		
	].

]
