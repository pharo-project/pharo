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
			spec package: 'Shift-ClassInstaller' with:[spec requires: #('Shift-ClassBuilder')].
			spec package: 'Shift-ClassBuilder-Tests' with:[spec requires: #core].
			spec package: 'Shift-ClassInstaller-Tests' with:[spec requires: #core].

			spec
				group: 'core' with: #('Shift-ClassBuilder' 'Shift-ClassInstaller');
				group: 'dev' with: #('core' 'tests');
				group: 'shift-tests' with: #('Shift-ClassBuilder-Tests' 'Shift-ClassInstaller-Tests');
				group: 'tests' with: #('shift-tests');
				group: 'default' with: #('core')
			]
]
