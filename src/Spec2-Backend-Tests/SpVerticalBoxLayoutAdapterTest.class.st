Class {
	#name : #SpVerticalBoxLayoutAdapterTest,
	#superclass : #SpBoxLayoutAdapterTest,
	#category : #'Spec2-Backend-Tests-Layout'
}

{ #category : #running }
SpVerticalBoxLayoutAdapterTest >> newLayout [

	^ SpBoxLayout newVertical
]
