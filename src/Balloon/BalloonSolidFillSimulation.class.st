"
This class is a simulation of the code that's run by the Balloon engine. For debugging purposes only.
"
Class {
	#name : #BalloonSolidFillSimulation,
	#superclass : #Object,
	#instVars : [
		'color'
	],
	#category : 'Balloon-Simulation'
}

{ #category : #computing }
BalloonSolidFillSimulation >> computeFillFrom: minX to: maxX at: yValue in: form [
	| bb |
	color isTransparent ifFalse:[
		bb := BitBlt toForm: form.
		bb fillColor: color.
		bb destX: 0 destY: 0 width: (maxX - minX) height: 1.
		bb combinationRule: Form over.
		bb copyBits].
]

{ #category : #computing }
BalloonSolidFillSimulation >> computeInitialStateFrom: source with: aColorTransform [
	color := source asColor.
]
