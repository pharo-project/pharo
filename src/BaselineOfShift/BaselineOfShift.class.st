Class {
	#name : #BaselineOfShift,
	#superclass : #BaselineOf,
	#category : 'BaselineOfShift'
}

{ #category : #baseline }
BaselineOfShift >> baseline: spec [
	<baseline>
	spec
		for: #pharo
		do:
			[ 
			spec project: 'Ficus' with: [
				spec
					className: #ConfigurationOfFicus;
					versionString: #development;
					loads: #FicusMonticello;
					repository: 'http://smalltalkhub.com/mc/MartinDias/Ficus/main' ].
			
			spec package: 'Shift-Changes'.
			spec package: 'Shift-ClassBuilder' with:[spec requires: #('Shift-Changes')].
			spec package: 'Shift-ClassInstaller' with:[spec requires: #('Shift-ClassBuilder')].

			spec package: 'Shift-Changes-Ficus' with:[spec requires: #'Ficus'].

			spec package: 'Shift-Changes-Tests' with:[spec requires: #'core'].
			spec package: 'Shift-ClassBuilder-Tests' with:[spec requires: #'core'].
			spec package: 'Shift-ClassInstaller-Tests' with:[spec requires: #'core'].

			spec package: 'Shift-Changes-Ficus-Tests' with:[spec requires: #('core' 'Shift-Changes-Ficus')].

			spec
				group: 'core' with: #('Shift-Changes' 'Shift-ClassBuilder' 'Shift-ClassInstaller');
				group: 'dev' with: #('core' 'tests');
				group: 'shift-tests' with: #('Shift-Changes-Tests' 'Shift-ClassBuilder-Tests' 'Shift-ClassInstaller-Tests');
				group: 'shift-ficus-tests' with: #('Shift-Changes-Ficus-Tests'); 
				group: 'tests' with: #('shift-tests' 'shift-ficus-tests');
				group: 'default' with: #('core')
			]
]
