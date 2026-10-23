Class {
	#name : 'BaselineOfSlotExtensions',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfSlotExtensions',
	#package : 'BaselineOfSlotExtensions'
}

{ #category : 'baselines' }
BaselineOfSlotExtensions class >> corePackages [
	| groups |

	groups := self packagesOfGroupNamed: 'core-traits'.
	^ groups flatCollect: [ :e | self packagesOfGroupNamed: e ].
]

{ #category : 'baselines' }
BaselineOfSlotExtensions >> baseline: spec [
	<baseline>
	| repository |
	repository := self packageRepositoryURLForSpec: spec.
	
	spec for: #common do: [ 
		spec
			baseline: 'Shift' with: [ spec loads: 'Core' from: repository ];
			package: 'Kernel-CodeModel-Tests';
			package: 'Slot-Examples';
			package: 'Slot-Examples-Tests'  with: [ spec requires: #('Slot-Examples' 'Kernel-CodeModel-Tests') ];
			package: 'SlotExtensions' with: [ spec requires: #('Shift') ];
			package: 'SlotExtensions-Tests' with: [ spec requires: #('SlotExtensions') ].

		spec
			group: 'Core' with: #('Slot-Examples' 'SlotExtensions');
			group: 'Tests' with: #('Slot-Examples-Tests' 'SlotExtensions-Tests') 
	]
]
