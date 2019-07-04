Class {
	#name : #SpSliderMark,
	#superclass : #Object,
	#instVars : [
		'value',
		'text'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #accessing }
SpSliderMark >> text [
	^ text
]

{ #category : #accessing }
SpSliderMark >> text: aText [
	text := aText
]

{ #category : #accessing }
SpSliderMark >> value [
	^ value
]

{ #category : #accessing }
SpSliderMark >> value: aValue [
	value := aValue
]
