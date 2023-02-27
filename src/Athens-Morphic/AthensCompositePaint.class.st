"
i am used to convert CompositeFillStyle into paint, used in Morphic.
"
Class {
	#name : #AthensCompositePaint,
	#superclass : #AthensPaint,
	#instVars : [
		'paints'
	],
	#category : #'Athens-Morphic'
}

{ #category : #drawing }
AthensCompositePaint >> athensFillPath: athensPath on: anAthensCanvas [

	"This is a terminal method in rendering dispatch scheme
	canvas->shape->paint. See AthensCanvas>>fillShape: "

	paints do: [:paint |
				paint athensFillPath: athensPath on: anAthensCanvas ]
]

{ #category : #drawing }
AthensCompositePaint >> athensFillRectangle: aRect on: anAthensCanvas [
	"This is a terminal method in rendering dispatch scheme
	canvas->shape->paint. See AthensCanvas>>fillShape: "


	paints do: [:paint |
				paint athensFillRectangle: aRect on: anAthensCanvas ]
]

{ #category : #initialization }
AthensCompositePaint >> fromFillStyles: fillStyles on: aCanvas [

	paints := fillStyles collect: [:each | each asAthensPaintOn: aCanvas ]
]
