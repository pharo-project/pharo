Class {
	#name : #SpLayoutDirectionVertical,
	#superclass : #SpLayoutDirection,
	#category : #'Spec2-Layout-Base'
}

{ #category : #testing }
SpLayoutDirectionVertical >> isVertical [

	^ true
]

{ #category : #factory }
SpLayoutDirectionVertical >> newWidgetOn: anAdapter [

	^ anAdapter newVertical
]
