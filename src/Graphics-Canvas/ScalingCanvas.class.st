Class {
	#name : #ScalingCanvas,
	#superclass : #PluggableCanvas,
	#instVars : [
		'formCanvas',
		'scale'
	],
	#category : #'Graphics-Canvas-Canvases'
}

{ #category : #examples }
ScalingCanvas class >> example [

	| scale morph extent baseForm form1 form2 |

	scale := 2.
	(morph := MenuMorph new)
		addTitle: 'Lorem ipsum'.
	(morph add: 'Dolor sit amet' target: nil selector: #yourself) keyText: 'x'.
	(morph add: 'Consectetur adipiscing elit' target: nil selector: #yourself) keyText: 'y'.
	morph addLine.
	(morph add: 'Sed do eiusmod' target: nil selector: #yourself) keyText: 'z'.
	extent := morph fullBounds bottomRight.
	
	(baseForm := Form extent: extent depth: Display depth)
		fillColor: Smalltalk ui theme backgroundColor.
	baseForm getCanvas fullDrawMorph: morph.
	form1 := baseForm scaledToSize: extent * scale.
	
	(form2 := Form extent: extent * scale depth: Display depth)
		fillColor: Smalltalk ui theme backgroundColor.
	(self formCanvas: form2 getCanvas scale: scale) fullDrawMorph: morph.

	{ morph. form1. form2 } inspect
]

{ #category : #'instance creation' }
ScalingCanvas class >> formCanvas: formCanvas scale: scale [

	^ self new formCanvas: formCanvas scale: scale
]

{ #category : #private }
ScalingCanvas >> apply: aBlock [

	FreeTypeSettings current forceNonSubPixelDuring: [
		formCanvas transformBy: (MatrixTransform2x3 withScale: scale)
			clippingTo: formCanvas clipRect
			during: aBlock ]
]

{ #category : #private }
ScalingCanvas >> drawString: s from: firstIndex to: lastIndex in: boundsRect font: fontOrNil color: c [

	formCanvas drawString: s from: firstIndex to: lastIndex in: (boundsRect scaleBy: scale)
		font: fontOrNil color: c scale: scale
]

{ #category : #initialization }
ScalingCanvas >> formCanvas: initialFormCanvas scale: initialScale [

	formCanvas := initialFormCanvas.
	scale := initialScale.

]
