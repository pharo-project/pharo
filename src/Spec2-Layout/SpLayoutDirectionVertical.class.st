Class {
	#name : #SpLayoutDirectionVertical,
	#superclass : #SpLayoutDirection,
	#category : #'Spec2-Layout-Base'
}

{ #category : #factory }
SpLayoutDirectionVertical >> newWidgetOn: anAdapter [

	^ anAdapter newVertical
]
