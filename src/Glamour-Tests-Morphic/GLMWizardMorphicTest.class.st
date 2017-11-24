Class {
	#name : #GLMWizardMorphicTest,
	#superclass : #GLMMorphicTest,
	#category : #'Glamour-Tests-Morphic'
}

{ #category : #tests }
GLMWizardMorphicTest >> testClose [
	| wizard |
	wizard := GLMWizard new.
	(wizard genericStep: #step) show: [:a | a text ] ; size: 2.
	window := wizard openOn: 123.
	self assert: window owner notNil.
	wizard close.
	self assert: window owner isNil
]
