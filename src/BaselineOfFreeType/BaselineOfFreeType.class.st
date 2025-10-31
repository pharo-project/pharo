Class {
	#name : 'BaselineOfFreeType',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfFreeType',
	#package : 'BaselineOfFreeType'
}

{ #category : 'baselines' }
BaselineOfFreeType >> baseline: spec [

	<baseline>
	spec for: #common do: [
			spec
				package: 'FreeType';
				package: 'FreeType-Graphics' with: [ spec requires: #( 'FreeType' ) ];
				package: 'EmbeddedFreeType' with: [ spec requires: #( 'FreeType' ) ];
				package: 'EmbeddedFreeType-Tests' with: [ spec requires: #( 'EmbeddedFreeType' ) ];
				package: 'FreeType-Tests' with: [ spec requires: #( 'FreeType' ) ].

			spec
				group: 'core' with: #( 'FreeType' 'EmbeddedFreeType' );
				group: 'ui' with: #( 'core' 'FreeType-Graphics' );
				group: 'tests' with: #( 'core' 'FreeType-Tests' 'EmbeddedFreeType-Tests' ) ]
]

{ #category : 'accessing' }
BaselineOfFreeType >> project [
	
	| aProject |
	
	aProject := super project.
	aProject loadType: #atomic.
	^ aProject
]
