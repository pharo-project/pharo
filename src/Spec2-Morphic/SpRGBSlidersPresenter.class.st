"
I am an elementary Spec widget allowing a user to choose a Color by its RGB values.

self example

I manage three SliderInput.
I set their min at 0 and their max at 255.

I provide the methods color that calculate the Color obtains with the current value of the sliders.
"
Class {
	#name : #SpRGBSlidersPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'redSlider',
		'greenSlider',
		'blueSlider'
	],
	#category : #'Spec2-Morphic-Widgets'
}

{ #category : #specs }
SpRGBSlidersPresenter class >> defaultSpec [
	^ SpBoxLayout newTopToBottom
		add: #redSlider;
		add: #greenSlider;
		add: #blueSlider;
		yourself
]

{ #category : #accessing }
SpRGBSlidersPresenter >> blueSlider [
	^ blueSlider
]

{ #category : #accessing }
SpRGBSlidersPresenter >> color [
	^ (Color
				r: redSlider value
				g: greenSlider value
				b: blueSlider value
				range: 255)
]

{ #category : #accessing }
SpRGBSlidersPresenter >> greenSlider [
	^ greenSlider
]

{ #category : #initialization }
SpRGBSlidersPresenter >> initializePresenters [
	redSlider := self instantiate: SpSliderInputPresenter.
	greenSlider := self instantiate: SpSliderInputPresenter.
	blueSlider := self instantiate: SpSliderInputPresenter.
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
SpRGBSlidersPresenter >> redSlider [
	^ redSlider
]

{ #category : #initialization }
SpRGBSlidersPresenter >> whenChangedDo: aBlock [
	redSlider whenValueChangedDo: aBlock.
	greenSlider whenValueChangedDo: aBlock.
	blueSlider whenValueChangedDo: aBlock
]
