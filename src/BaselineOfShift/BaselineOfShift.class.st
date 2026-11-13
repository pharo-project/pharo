Class {
	#name : 'BaselineOfShift',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfShift',
	#package : 'BaselineOfShift'
}

{ #category : 'baselines' }
BaselineOfShift >> baseline: spec [
	<baseline>
	spec for: #pharo do: [
			spec 
				package: 'Kernel-CodeModel-Tests';
				package: 'Shift-ClassBuilder';
				package: 'Shift-ClassBuilder-Tests' with:[ spec requires: 'Kernel-CodeModel-Tests' ];

				group: 'Core' with: #('Shift-ClassBuilder');
				group: 'dev' with: #('Core' 'Tests');
				group: 'Tests' with: #('Shift-ClassBuilder-Tests');
				group: 'default' with: #('Core')
			]
]
