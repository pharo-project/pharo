"
This baseline is used to generate Pharo images from the local clone of pharo-project/pharo repository. 

Depends on:
- BaselineOfSUnit

Load type: 
- linear

Installs:
- Graphics-* packages (Form, Bitmap, GrafPort...)
"
Class {
	#name : 'BaselineOfDisplay',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfDisplay',
	#package : 'BaselineOfDisplay'
}

{ #category : 'baseline' }
BaselineOfDisplay >> baseline: spec [

	<baseline>
	| repository |
	repository := self packageRepositoryURLForSpec: spec.
	spec for: #common do: [
			"The BMP tests decode their images from Base64"
			spec baseline: 'ZincCharacterEncoding' with: [ spec loads: #Core from: repository ].

			spec
				package: 'Colors';
				package: 'Graphics-Display Objects';
				package: 'Graphics-Primitives' with: [ spec requires: 'Graphics-Display Objects' ];
				package: 'Graphics-Transformations';
				package: 'Graphics-Files';
				package: 'Graphics-Tests' with: [ spec requires: #( 'Graphics-Primitives' 'Graphics-Files' 'ZincCharacterEncoding' ) ].
			spec
				group: 'default' with: #( 'Core' 'Graphics-Files' );
				group: 'Core' with: #( 'Colors' 'Graphics-Display Objects' 'Graphics-Primitives' 'Graphics-Transformations' );
				group: 'Tests' with: #( 'default' 'Graphics-Tests' ) ]
]

{ #category : 'accessing' }
BaselineOfDisplay >> project [
	"We currently have cyclic dependencies so we are loading this project atomically."

	^ super project
		  loadType: #atomic;
		  yourself
]
