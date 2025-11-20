Class {
	#name : 'BaselineOfUIInfrastructure',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfUIInfrastructure',
	#package : 'BaselineOfUIInfrastructure'
}

{ #category : 'baselines' }
BaselineOfUIInfrastructure >> baseline: spec [

	<baseline>
	| repository |
	repository := self packageRepositoryURLForSpec: spec.
	spec for: #common do: [
			spec
				baseline: 'Display' with: [ spec repository: repository ];
				baseline: 'Fonts' with: [ spec loads: #Core from: repository ];
				baseline: 'FreeType' with: [ spec loads: #ui from: repository ];
				baseline: 'MenuRegistration' with: [ spec repository: repository ];
				baseline: 'Keymapping' with: [ spec loads: #Core from: repository ].

			spec package: 'System-Utilities' ]
]
