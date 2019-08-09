"
I am a LabelledContainer specialized to manage a SliderInput.
See LabelledContainer

self example

I provide the accessors sliderInput to be more intuitive than subwidget, and the shortcuts input and slider.

I provide shortcuts for the most common methods to avoid the use of the DoesNotUnderstand or the writing of long chain of accessors.
"
Class {
	#name : #SpLabelledSliderInputPresenter,
	#superclass : #SpLabelledContainerPrestenter,
	#category : #'Spec2-PolyWidgets-Labelled'
}

{ #category : #'api-shortcuts' }
SpLabelledSliderInputPresenter >> absoluteValue [
	^ self sliderInput absoluteValue
]

{ #category : #'api-shortcuts' }
SpLabelledSliderInputPresenter >> absoluteValue: aBoolean [
	^ self sliderInput absoluteValue: aBoolean
]

{ #category : #'api-shortcuts' }
SpLabelledSliderInputPresenter >> autoAccept [
	^ self sliderInput autoAccept
]

{ #category : #'api-shortcuts' }
SpLabelledSliderInputPresenter >> autoAccept: aBoolean [
	^ self sliderInput autoAccept: aBoolean
]

{ #category : #initialization }
SpLabelledSliderInputPresenter >> initializeWidgets [
	super initializeWidgets.
	self content: SpSliderInputPresenter 
]

{ #category : #accessing }
SpLabelledSliderInputPresenter >> input [
	^ subwidget input
]

{ #category : #'api-shortcuts' }
SpLabelledSliderInputPresenter >> max: aNumber [
	^ self sliderInput max: aNumber
]

{ #category : #'api-shortcuts' }
SpLabelledSliderInputPresenter >> min: aNumber [
	^ self sliderInput min: aNumber
]

{ #category : #accessing }
SpLabelledSliderInputPresenter >> slider [
	^ subwidget slider
]

{ #category : #accessing }
SpLabelledSliderInputPresenter >> sliderInput [
	^ subwidget
]

{ #category : #accessing }
SpLabelledSliderInputPresenter >> value [
	^ self sliderInput value
]

{ #category : #accessing }
SpLabelledSliderInputPresenter >> value: aNumber [
	^ self sliderInput value: aNumber
]

{ #category : #'api-events' }
SpLabelledSliderInputPresenter >> whenValueChangedDo: aBlock [
	self sliderInput whenValueChangedDo: aBlock
]
