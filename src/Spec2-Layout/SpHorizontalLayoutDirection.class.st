Class {
	#name : #SpHorizontalLayoutDirection,
	#superclass : #SpLayoutDirection,
	#category : #'Spec2-Layout-Base'
}

{ #category : #testing }
SpHorizontalLayoutDirection >> isHorizontal [

	^ true
]

{ #category : #factory }
SpHorizontalLayoutDirection >> newWidgetOn: anAdapter [

	^ anAdapter newHorizontal
]
