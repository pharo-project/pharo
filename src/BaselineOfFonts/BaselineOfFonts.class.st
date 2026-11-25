Class {
	#name : 'BaselineOfFonts',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfFonts',
	#package : 'BaselineOfFonts'
}

{ #category : 'accessing' }
BaselineOfFonts class >> corePackageNames [

	^ self packagesOfGroupNamed: #Core
]

{ #category : 'baselines' }
BaselineOfFonts >> baseline: spec [

	<baseline>
	| repository |
	repository := self packageRepositoryURLForSpec: spec.
	spec for: #common do: [
			"Fonts draw on forms"
			spec baseline: 'Display' with: [ spec repository: repository ].

			spec
				package: 'Fonts-Abstract';
				package: 'Fonts-Infrastructure' with: [ spec requires: #( 'Fonts-Abstract' 'Text-Core' ) ];
				package: 'Fonts-Infrastructure-Tests' with: [ spec requires: #( 'Fonts-Infrastructure' ) ];
				package: 'Text-Core' with: [ spec requires: #( 'Fonts-Abstract' 'Display' ) ];
				package: 'Graphics-Fonts' with: [ spec requires: #( 'Fonts-Abstract' 'Fonts-Infrastructure' 'Text-Core' 'Display' ) ].

			spec
				group: 'Core' with: #( 'Fonts-Abstract' 'Fonts-Infrastructure' 'Graphics-Fonts' 'Text-Core' );
				group: 'Tests' with: #( 'Fonts-Infrastructure-Tests' ) ]
]
