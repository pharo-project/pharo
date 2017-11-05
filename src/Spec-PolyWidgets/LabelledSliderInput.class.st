"
I am a LabelledContainer specialized to manage a SliderInput.
See LabelledContainer

self example

I provide the accessors sliderInput to be more intuitive than subwidget, and the shortcuts input and slider.

I provide shortcuts for the most common methods to avoid the use of the DoesNotUnderstand or the writing of long chain of accessors.
"
Class {
	#name : #LabelledSliderInput,
	#superclass : #LabelledContainer,
	#category : #Spec-PolyWidgets-Labelled
}

{ #category : #example }
LabelledSliderInput class >> example [
	| example |
	example := self new
		min: 0;
		max: 250;
		autoAccept: true;
		value: 120;
		extent: 400 @ 50;
		title: 'LabelledSliderInput example';
		openWithSpec.
	^ example
]

{ #category : #api-shortcuts }
LabelledSliderInput >> absoluteValue [
	^ self sliderInput absoluteValue
]

{ #category : #api-shortcuts }
LabelledSliderInput >> absoluteValue: aBoolean [
	^ self sliderInput absoluteValue: aBoolean
]

{ #category : #api-shortcuts }
LabelledSliderInput >> autoAccept [
	^ self sliderInput autoAccept
]

{ #category : #api-shortcuts }
LabelledSliderInput >> autoAccept: aBoolean [
	^ self sliderInput autoAccept: aBoolean
]

{ #category : #initialization }
LabelledSliderInput >> initializeWidgets [
	super initializeWidgets.
	self content: SliderInput 
]

{ #category : #accessing }
LabelledSliderInput >> input [
	^ subwidget input
]

{ #category : #api-shortcuts }
LabelledSliderInput >> max: aNumber [
	^ self sliderInput max: aNumber
]

{ #category : #api-shortcuts }
LabelledSliderInput >> min: aNumber [
	^ self sliderInput min: aNumber
]

{ #category : #accessing }
LabelledSliderInput >> slider [
	^ subwidget slider
]

{ #category : #accessing }
LabelledSliderInput >> sliderInput [
	^ subwidget
]

{ #category : #accessing }
LabelledSliderInput >> value [
	^ self sliderInput value
]

{ #category : #accessing }
LabelledSliderInput >> value: aNumber [
	^ self sliderInput value: aNumber
]

{ #category : #api-events }
LabelledSliderInput >> whenValueChangedDo: aBlock [
	self sliderInput whenValueChangedDo: aBlock
]
