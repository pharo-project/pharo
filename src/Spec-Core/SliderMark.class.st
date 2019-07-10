Class {
	#name : #SliderMark,
	#superclass : #Object,
	#instVars : [
		'value',
		'text'
	],
	#category : #'Spec-Core-Widgets'
}

{ #category : #accessing }
SliderMark >> text [
	^ text
]

{ #category : #accessing }
SliderMark >> text: aText [
	text := aText
]

{ #category : #accessing }
SliderMark >> value [
	^ value
]

{ #category : #accessing }
SliderMark >> value: aValue [
	value := aValue
]
