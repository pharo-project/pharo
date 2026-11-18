"
I check that each color selector morph tells its owner the colors selected, and that each one passes on what the morph inside it selects.
"
Class {
	#name : 'ColorSelectorMorphsTest',
	#superclass : 'TestCase',
	#instVars : [
		'selectedColors'
	],
	#category : 'Morphic-Widgets-ColorPicker-Tests',
	#package : 'Morphic-Widgets-ColorPicker-Tests'
}

{ #category : 'helpers' }
ColorSelectorMorphsTest >> record: aColorSelectorMorph [
	"Answer aColorSelectorMorph, recording from now on the colors it tells"

	^ aColorSelectorMorph
		  whenColorSelectedDo: [ :aColor | selectedColors add: aColor ];
		  yourself
]

{ #category : 'running' }
ColorSelectorMorphsTest >> setUp [

	super setUp.
	selectedColors := OrderedCollection new
]

{ #category : 'tests' }
ColorSelectorMorphsTest >> testHSVASelectorPassesOnTheColorOfItsHSVSelector [

	| selector |
	selector := self record: HSVAColorSelectorMorph new.

	selector hsvMorph colorSelected: Color red.

	self assert: selectedColors size equals: 1.
	self assert: selectedColors first equals: selector selectedColor
]

{ #category : 'tests' }
ColorSelectorMorphsTest >> testHSVSelectorPassesOnTheColorOfItsSVSelector [

	| selector |
	selector := self record: HSVColorSelectorMorph new.

	selector svMorph selectedColor: Color red.

	self assert: selectedColors asArray equals: { Color red }
]

{ #category : 'tests' }
ColorSelectorMorphsTest >> testSVSelectorTellsTheSelectedColor [

	| selector |
	selector := self record: SVColorSelectorMorph new.

	selector selectedColor: Color red.

	self assert: selectedColors asArray equals: { Color red }
]

{ #category : 'tests' }
ColorSelectorMorphsTest >> testSelectorWithoutBlockSelectsSilently [

	| selector |
	selector := SVColorSelectorMorph new.

	selector selectedColor: Color red.

	self assert: selector selectedColor equals: Color red
]
