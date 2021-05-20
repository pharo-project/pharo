Class {
	#name : #SpRGBSlidersPresenterTest,
	#superclass : #SpSmokeTest,
	#category : #'Spec2-Morphic-Tests'
}

{ #category : #running }
SpRGBSlidersPresenterTest >> classToTest [
	^ SpRGBSlidersPresenter
]

{ #category : #tests }
SpRGBSlidersPresenterTest >> testColor [

	| random red green blue |
	random := Random new.
	red := random nextInteger: 255.
	green := random nextInteger: 255.
	blue := random nextInteger: 255.

	presenter redSlider value: red.
	presenter greenSlider value: green.
	presenter blueSlider value: blue.
	self assert: presenter color equals: (Color
			 r: red
			 g: green
			 b: blue
			 range: 255)
]

{ #category : #tests }
SpRGBSlidersPresenterTest >> testwhenChangedDo [
	| numberOfChanges |
	numberOfChanges := 0.
	presenter whenChangedDo: [ numberOfChanges := numberOfChanges + 1 ].
	presenter redSlider value: 66.
	presenter greenSlider value: 17.
	presenter blueSlider value: 200.
	self assert: numberOfChanges equals: 3
]
