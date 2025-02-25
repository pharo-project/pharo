Class {
	#name : 'BaselineOfShift',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfShift',
	#package : 'BaselineOfShift'
}

{ #category : 'baselines' }
BaselineOfShift >> baseline: spec [
	<baseline>
	spec
		for: #pharo
		do:
			[
			spec package: 'Shift-ClassBuilder'.
			spec package: 'Shift-ClassBuilder-Tests' with:[spec requires: #core].

			spec
				group: 'core' with: #('Shift-ClassBuilder');
				group: 'dev' with: #('core' 'tests');
				group: 'shift-tests' with: #('Shift-ClassBuilder-Tests');
				group: 'tests' with: #('shift-tests');
				group: 'default' with: #('core')
			]
]
