"
This baseline is used to generate Pharo images from the local clone of pharo-project/pharo repository. 

Depends on:
- SUnit, Display, UnifiedFFI

Load type: 
- linear

Installs:
- minimal working Morphic subset without fonts support
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
			spec package: 'System-Object Events'.
			spec package: 'System-Caching'.
			spec package: 'Graphics-Canvas'.
			spec package: 'FormCanvas-Core'.
			spec package: 'System-Model'.
			spec package: 'DeepCopier'.
			spec package: 'FileSystem-Zip'.
			spec package: 'Morphic-Core' with: [ spec requires: #( 'System-Model' 'DeepCopier') ] ]
]
