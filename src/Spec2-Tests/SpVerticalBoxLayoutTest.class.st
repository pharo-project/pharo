Class {
	#name : #SpVerticalBoxLayoutTest,
	#superclass : #SpBoxLayoutTest,
	#category : #'Spec2-Tests-Layout'
}

{ #category : #running }
SpVerticalBoxLayoutTest >> initializeTestedInstance [

	layout := SpBoxLayout newVertical.
	presenter layout: layout
]
