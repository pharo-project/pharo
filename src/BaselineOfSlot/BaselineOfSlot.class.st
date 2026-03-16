Class {
	#name : 'BaselineOfSlot',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfSlot',
	#package : 'BaselineOfSlot'
}

{ #category : 'baselines' }
BaselineOfSlot class >> corePackages [
	| groups |

	groups := self packagesOfGroupNamed: 'core-traits'.
	^ groups flatCollect: [ :e | self packagesOfGroupNamed: e ].
]

{ #category : 'baselines' }
BaselineOfSlot >> baseline: spec [
	<baseline>
	spec
		for: #common
		do: [ spec
				package: 'Slot-Examples';
				package: 'Slot-Tests' with: [ spec requires: #('Slot-Examples') ];
				package: 'VariablesLibrary';
				package: 'VariablesLibrary-Tests' with: [ spec requires: #('VariablesLibrary' 'Slot-Tests') ].

			spec
				group: 'core' with: #('VariablesLibrary');
				group: 'slot-tests' with: #('Slot-Tests' 'VariablesLibrary-Tests') ]
]
