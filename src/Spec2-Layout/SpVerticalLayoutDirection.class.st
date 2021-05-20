Class {
	#name : #SpVerticalLayoutDirection,
	#superclass : #SpLayoutDirection,
	#category : #'Spec2-Layout-Base'
}

{ #category : #testing }
SpVerticalLayoutDirection >> isVertical [

	^ true
]

{ #category : #factory }
SpVerticalLayoutDirection >> newWidgetOn: anAdapter [

	^ anAdapter newVertical
]
