"
I am a LabelledContainer specialized to manage a SliderInput.
See LabelledContainer

self example

I provide the accessors sliderInput to be more intuitive than subwidget, and the shortcuts input and slider.

I provide shortcuts for the most common methods to avoid the use of the DoesNotUnderstand or the writing of long chain of accessors.
"
Class {
	#name : #SpLabelledSliderInput,
	#superclass : #SpLabelledContainer,
	#category : #'Spec2-Core-Widgets'
}

{ #category : #'api-shortcuts' }
SpLabelledSliderInput >> absoluteValue [
	^ self sliderInput absoluteValue
]

{ #category : #'api-shortcuts' }
SpLabelledSliderInput >> absoluteValue: aBoolean [
	^ self sliderInput absoluteValue: aBoolean
]

{ #category : #'api-shortcuts' }
SpLabelledSliderInput >> autoAccept [
	^ self sliderInput autoAccept
]

{ #category : #'api-shortcuts' }
SpLabelledSliderInput >> autoAccept: aBoolean [
	^ self sliderInput autoAccept: aBoolean
]

{ #category : #initialization }
SpLabelledSliderInput >> initializePresenters [
	super initializePresenters.
	self content: SpSliderInput 
]

{ #category : #accessing }
SpLabelledSliderInput >> input [
	^ subwidget input
]

{ #category : #'api-shortcuts' }
SpLabelledSliderInput >> max: aNumber [
	^ self sliderInput max: aNumber
]

{ #category : #'api-shortcuts' }
SpLabelledSliderInput >> min: aNumber [
	^ self sliderInput min: aNumber
]

{ #category : #accessing }
SpLabelledSliderInput >> slider [
	^ subwidget slider
]

{ #category : #accessing }
SpLabelledSliderInput >> sliderInput [
	^ subwidget
]

{ #category : #accessing }
SpLabelledSliderInput >> value [
	^ self sliderInput value
]

{ #category : #accessing }
SpLabelledSliderInput >> value: aNumber [
	^ self sliderInput value: aNumber
]

{ #category : #'api-events' }
SpLabelledSliderInput >> whenValueChangedDo: aBlock [
	self sliderInput whenValueChangedDo: aBlock
]
