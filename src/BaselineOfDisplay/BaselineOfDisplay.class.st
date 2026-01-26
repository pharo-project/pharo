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
	spec for: #common do: [
			spec
				package: 'Colors';
				package: 'Graphics-Display Objects';
				package: 'Graphics-Primitives' with: [ spec requires: 'Graphics-Display Objects' ];
				package: 'Graphics-Transformations';
				package: 'Graphics-Files' ]
]

{ #category : 'accessing' }
BaselineOfDisplay >> project [
	"We currently have cyclic dependencies so we are loading this project atomically."

	^ super project
		  loadType: #atomic;
		  yourself
]
