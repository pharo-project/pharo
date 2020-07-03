Class {
	#name : #SpLayoutDirectionHorizontal,
	#superclass : #SpLayoutDirection,
	#category : #'Spec2-Layout-Base'
}

{ #category : #testing }
SpLayoutDirectionHorizontal >> isHorizontal [

	^ true
]

{ #category : #factory }
SpLayoutDirectionHorizontal >> newWidgetOn: anAdapter [

	^ anAdapter newHorizontal
]
