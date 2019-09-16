"
I am an elementary Spec widget allowing a user to choose a Color by its RGB values.

self example

I manage three SliderInput.
I set their min at 0 and their max at 255.

I provide the methods color that calculate the Color obtains with the current value of the sliders.
"
Class {
	#name : #SpRGBSliders,
	#superclass : #SpPresenter,
	#instVars : [
		'redSlider',
		'greenSlider',
		'blueSlider'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpRGBSliders class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #redSlider;
		add: #greenSlider;
		add: #blueSlider;
		yourself
]

{ #category : #accessing }
SpRGBSliders >> blueSlider [
	^ blueSlider
]

{ #category : #accessing }
SpRGBSliders >> color [
	^ (Color
				r: redSlider value
				g: greenSlider value
				b: blueSlider value
				range: 255)
]

{ #category : #accessing }
SpRGBSliders >> greenSlider [
	^ greenSlider
]

{ #category : #initialization }
SpRGBSliders >> initializePresenters [
	redSlider := self instantiate: SpSliderInput.
	greenSlider := self instantiate: SpSliderInput.
	blueSlider := self instantiate: SpSliderInput.
	redSlider
		min: 0;
		max: 255; label: 'Red' translated.
	greenSlider
		min: 0;
		max: 255; label: 'Green' translated.
	blueSlider
		min: 0;
		max: 255; label: 'Blue' translated.
	self focusOrder
		add: redSlider;
		add: greenSlider;
		add: blueSlider
]

{ #category : #accessing }
SpRGBSliders >> redSlider [
	^ redSlider
]

{ #category : #initialization }
SpRGBSliders >> whenChangedDo: aBlock [
	redSlider whenValueChangedDo: aBlock.
	greenSlider whenValueChangedDo: aBlock.
	blueSlider whenValueChangedDo: aBlock
]
