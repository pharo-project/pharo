Class {
	#name : #RGBSlidersTest,
	#superclass : #SpecTestCase,
	#category : #'Spec-Tests-PolyWidgets-Support'
}

{ #category : #accessing }
RGBSlidersTest >> classToTest [
	^ RGBSliders
]

{ #category : #tests }
RGBSlidersTest >> testColor [
	| random red green blue |
	random := Random new.
	red := random nextInteger: 255.
	green := random nextInteger: 255.
	blue := random nextInteger: 255.

	testedInstance redSlider value: red.
	testedInstance greenSlider value: green.
	testedInstance blueSlider value: blue.

	self
		assert: testedInstance color
		equals:
			(Color
				r: red
				g: green
				b: blue
				range: 255)
]

{ #category : #tests }
RGBSlidersTest >> testwhenChangedDo [
	| numberOfChanges |
	numberOfChanges := 0.
	testedInstance whenChangedDo: [ numberOfChanges := numberOfChanges + 1 ].
	testedInstance redSlider value: 66.
	testedInstance greenSlider value: 17.
	testedInstance blueSlider value: 200.
	self assert: numberOfChanges equals: 3
]
