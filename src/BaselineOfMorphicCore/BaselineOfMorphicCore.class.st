"
This baseline is used to generate Pharo images from the local clone of pharo-project/pharo repository. 

Depends on:
- Display, Fonts (Core), FreeType (core), Keymapping (Core), UIManager

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
				baseline: 'FreeType' with: [ spec loads: #core from: repository ];
				baseline: 'Keymapping' with: [ spec loads: #Core from: repository ];
				baseline: 'UIManager' with: [ spec repository: repository ].

			spec package: 'System-Object Events'.
			spec package: 'System-Caching'.
			spec package: 'Graphics-Canvas' with: [ spec requires: #( 'Display' ) ].
			spec package: 'FormCanvas-Core' with: [ spec requires: #( 'Graphics-Canvas' 'Fonts' ) ].
			spec package: 'System-Model'.
			spec package: 'FileSystem-Zip'.
			spec package: 'Morphic-Core' with: [
				spec requires: #( 'System-Object Events' 'System-Caching' 'FormCanvas-Core' 'System-Model' 'FileSystem-Zip' 'FreeType' 'Keymapping' 'UIManager' ) ] ]
]
