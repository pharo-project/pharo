Class {
	#name : #SpecLayoutDirectionHorizontal,
	#superclass : #SpecLayoutDirection,
	#category : #'Spec-Layout-Base'
}

{ #category : #factory }
SpecLayoutDirectionHorizontal >> newWidgetOn: anAdapter [

	^ anAdapter newHorizontal
]
