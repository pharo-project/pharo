"
I am an elementary Spec widget allowing a user to choose a Color by its RGB values.

self example

I manage three SliderInput.
I set their min at 0 and their max at 255.

I provide the methods color that calculate the Color obtains with the current value of the sliders.
"
Class {
	#name : #RGBSliders,
	#superclass : #ComposablePresenter,
	#instVars : [
		'redSlider',
		'greenSlider',
		'blueSlider'
	],
	#category : #'Spec-PolyWidgets-Support'
}

{ #category : #specs }
RGBSliders class >> defaultSpec [
	<spec: #default>
	^ SpecLayout composed
		newColumn: [ :column | 
			column
				add: #redSlider;
				add: #greenSlider;
				add: #blueSlider ];
		yourself
]

{ #category : #example }
RGBSliders class >> example [
	<sampleInstance>
	| example |
	example := self new.
	example
		title: 'RGBSliders example';
		extent: 300 @ 200;
		openDialogWithSpec.
	^ example
]

{ #category : #accessing }
RGBSliders >> blueSlider [
	^ blueSlider
]

{ #category : #accessing }
RGBSliders >> color [
	^ (Color
				r: redSlider value
				g: greenSlider value
				b: blueSlider value
				range: 255)
]

{ #category : #accessing }
RGBSliders >> greenSlider [
	^ greenSlider
]

{ #category : #initialization }
RGBSliders >> initializeWidgets [
	redSlider := self instantiate: SliderInput.
	greenSlider := self instantiate: SliderInput.
	blueSlider := self instantiate: SliderInput.
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
RGBSliders >> redSlider [
	^ redSlider
]

{ #category : #initialization }
RGBSliders >> whenChangedDo: aBlock [
	redSlider whenValueChangedDo: aBlock.
	greenSlider whenValueChangedDo: aBlock.
	blueSlider whenValueChangedDo: aBlock
]
