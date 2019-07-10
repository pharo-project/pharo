Class {
	#name : #SpLayoutDirectionHorizontal,
	#superclass : #SpLayoutDirection,
	#category : #'Spec2-Layout-Base'
}

{ #category : #factory }
SpLayoutDirectionHorizontal >> newWidgetOn: anAdapter [

	^ anAdapter newHorizontal
]
