Class {
	#name : #BaselineOfSlot,
	#superclass : #BaselineOf,
	#category : #BaselineOfSlot
}

{ #category : #baselines }
BaselineOfSlot class >> corePackages [
	| groups |

	groups := self packagesOfGroupNamed: 'core-traits'.
	^ groups flatCollect: [ :e | self packagesOfGroupNamed: e ].
]

{ #category : #baselines }
BaselineOfSlot >> baseline: spec [
	<baseline>

	spec for: #common do: [

		spec 
			package: 'Slot-Core';
			package: 'Slot-Examples' with: [ spec requires: #('Slot-Core') ];
			package: 'Slot-Tests' with: [ spec requires: #('Slot-Core' 'Slot-Examples') ].

		spec group: 'core' with: #('Slot-Core').
	
		spec group: 'default' with: #('core' 'slot-tests').
		spec group: 'slot-tests' with: #('Slot-Tests').		
	 ].	

]
