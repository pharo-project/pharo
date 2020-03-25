Class {
	#name : #SpVerticalPanedLayoutTest,
	#superclass : #SpPanedLayoutTest,
	#category : #'Spec2-Tests-Layout'
}

{ #category : #running }
SpVerticalPanedLayoutTest >> initializeTestedInstance [

	layout := SpPanedLayout newVertical.
	presenter layout: layout
]
