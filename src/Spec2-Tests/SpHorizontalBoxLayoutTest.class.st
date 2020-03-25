Class {
	#name : #SpHorizontalBoxLayoutTest,
	#superclass : #SpBoxLayoutTest,
	#category : #'Spec2-Tests-Layout'
}

{ #category : #running }
SpHorizontalBoxLayoutTest >> initializeTestedInstance [

	layout := SpBoxLayout newHorizontal.
	presenter layout: layout
]
