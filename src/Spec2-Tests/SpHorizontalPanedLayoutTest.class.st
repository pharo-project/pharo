Class {
	#name : #SpHorizontalPanedLayoutTest,
	#superclass : #SpPanedLayoutTest,
	#category : #'Spec2-Tests-Layout'
}

{ #category : #running }
SpHorizontalPanedLayoutTest >> initializeTestedInstance [

	layout := SpPanedLayout newLeftToRight.
	presenter layout: layout
]
