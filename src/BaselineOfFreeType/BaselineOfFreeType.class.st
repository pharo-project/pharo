Class {
	#name : 'BaselineOfFreeType',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfFreeType',
	#package : 'BaselineOfFreeType'
}

{ #category : 'accessing' }
BaselineOfFreeType class >> uiPackageNames [

	^ self packagesOfGroupNamed: #ui
]

{ #category : 'baselines' }
BaselineOfFreeType >> baseline: spec [

	<baseline>
	| repository |
	repository := self packageRepositoryURLForSpec: spec.
	spec for: #common do: [
			spec
				baseline: 'UnifiedFFI' with: [ spec loads: #minimal from: repository ];
				baseline: 'Display' with: [ spec loads: #Core from: repository ];
				baseline: 'Fonts' with: [ spec loads: #Core from: repository ];
				baseline: 'ZincCharacterEncoding' with: [ spec loads: #Core from: repository ].

			spec
				package: 'FreeType' with: [ spec requires: #( 'UnifiedFFI' 'Display' 'Fonts' ) ];
				package: 'FreeType-Graphics' with: [ spec requires: #( 'FreeType' 'ZincCharacterEncoding' ) ];
				package: 'EmbeddedFreeType' with: [ spec requires: #( 'FreeType' 'ZincCharacterEncoding' ) ];
				package: 'EmbeddedFreeType-Tests' with: [ spec requires: #( 'EmbeddedFreeType' ) ];
				package: 'FreeType-Tests' with: [ spec requires: #( 'FreeType-Graphics' 'EmbeddedFreeType' ) ].

			"FreeTypeFont renders its glyphs with FreeType-Graphics, so the core needs it"
			spec
				group: 'core' with: #( 'FreeType' 'EmbeddedFreeType' 'FreeType-Graphics' );
				group: 'ui' with: #( 'core' );
				group: 'Tests' with: #( 'core' 'FreeType-Tests' 'EmbeddedFreeType-Tests' ) ]
]

{ #category : 'accessing' }
BaselineOfFreeType >> project [
	
	| aProject |
	
	aProject := super project.
	aProject loadType: #atomic.
	^ aProject
]
