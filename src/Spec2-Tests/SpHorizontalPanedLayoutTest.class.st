Class {
	#name : #SpHorizontalPanedLayoutTest,
	#superclass : #SpPanedLayoutTest,
	#category : #'Spec2-Tests-Layout'
}

{ #category : #running }
SpHorizontalPanedLayoutTest >> initializeTestedInstance [

	layout := SpPanedLayout newHorizontal.
	presenter layout: layout
]
