Class {
	#name : 'BaselineOfUIInfrastructure',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfUIInfrastructure',
	#package : 'BaselineOfUIInfrastructure'
}

{ #category : 'baselines' }
BaselineOfUIInfrastructure >> baseline: spec [

	<baseline>
	spec for: #common do: [
		spec repository: (self packageRepositoryURLForSpec: spec).
			spec
				baseline: 'Display';
				baseline: 'Fonts' with: [ spec loads: #Core ];
				baseline: 'FreeType' with: [ spec loads: #ui ];
				baseline: 'MenuRegistration';
				baseline: 'Keymapping' with: [ spec loads: #Core ];
				baseline: 'Fuel' with: [ spec loads: #( 'Core' ) ]. "Fuel is currently depending on graphics objects so I'm loading it here for now instead of BaselineOfBasicTools."

			spec
				package: 'System-Utilities';
				package: 'System-FileRegistry';
				package: 'Tool-Registry'. ]
]
