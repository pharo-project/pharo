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
	spec
		for: #common
		do: [ spec
				package: 'Slot-Core';
				package: 'Slot-Examples' with: [ spec requires: #('Slot-Core') ];
				package: 'Slot-Tests' with: [ spec requires: #('Slot-Core' 'Slot-Examples') ];
				package: 'VariablesLibrary' with: [ spec requires: #('Slot-Core') ];
				package: 'VariablesLibrary-Tests' with: [ spec requires: #('VariablesLibrary' 'Slot-Tests') ].

			spec
				group: 'core' with: #('Slot-Core' 'VariablesLibrary');
				group: 'slot-tests' with: #('Slot-Tests' 'VariablesLibrary-Tests') ]
]
