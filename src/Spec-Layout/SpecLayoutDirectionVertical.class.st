Class {
	#name : #SpecLayoutDirectionVertical,
	#superclass : #SpecLayoutDirection,
	#category : #'Spec-Layout-Base'
}

{ #category : #factory }
SpecLayoutDirectionVertical >> newWidgetOn: anAdapter [

	^ anAdapter newVertical
]
