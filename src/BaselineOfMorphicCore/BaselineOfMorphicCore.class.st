"
This baseline is used to generate Pharo images from the local clone of pharo-project/pharo repository. 

Depends on:
- Display, Fonts (Core), Keymapping (Core), UIManager

Load type: 
- linear

Installs:
- minimal working Morphic subset: morphs, events, layouts, worlds and string morphs
"
Class {
	#name : 'BaselineOfMorphicCore',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfMorphicCore',
	#package : 'BaselineOfMorphicCore'
}

{ #category : 'baseline' }
BaselineOfMorphicCore >> baseline: spec [

	<baseline>
	| repository |
	repository := self packageRepositoryURLForSpec: spec.

	spec for: #common do: [
			spec
				baseline: 'Display' with: [ spec repository: repository ];
				baseline: 'Fonts' with: [ spec loads: #Core from: repository ];
				baseline: 'Keymapping' with: [ spec loads: #Core from: repository ];
				baseline: 'UIManager' with: [ spec repository: repository ].

			spec package: 'System-Object Events'.
			spec package: 'System-Caching'.
			spec package: 'Graphics-Canvas' with: [ spec requires: #( 'Display' ) ].
			spec package: 'FormCanvas-Core' with: [ spec requires: #( 'Graphics-Canvas' 'Fonts' ) ].
			spec package: 'System-Model'.
			spec package: 'FileSystem-Zip'.
			spec package: 'Morphic-Core' with: [
				spec
					requires: #( 'System-Object Events' 'System-Caching' 'FormCanvas-Core' 'System-Model' 'FileSystem-Zip' 'Keymapping' 'UIManager' );
					preLoadDoIt: #declareWorld:package: ].

			spec package: 'System-Object Events-Tests' with: [ spec requires: #( 'System-Object Events' ) ].
			spec package: 'System-Caching-Tests' with: [ spec requires: #( 'System-Caching' ) ].
			spec package: 'FormCanvas-Tests' with: [ spec requires: #( 'FormCanvas-Core' ) ].
			spec package: 'System-Model-Tests' with: [ spec requires: #( 'System-Model' ) ].

			spec
				group: 'default' with: #( 'Morphic-Core' );
				group: 'Tests' with: #( 'default' 'System-Object Events-Tests' 'System-Caching-Tests' 'FormCanvas-Tests' 'System-Model-Tests' ) ]
]

{ #category : 'actions' }
BaselineOfMorphicCore >> declareWorld: loader package: packageSpec [
	"Morphic-Core names the globals World and ActiveWorld, which exist only once a world is installed.
	Declare them before loading, so a strict load compiles. Never replace a world already there"

	#( #World #ActiveWorld ) do: [ :name |
		self class environment at: name ifAbsentPut: [ nil ] ]
]
