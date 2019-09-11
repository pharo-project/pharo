Class {
	#name : #SpLabelledContainerTest,
	#superclass : #SpSmokeTestCase,
	#category : #'Spec2-PolyWidgets-Tests-Tests'
}

{ #category : #running }
SpLabelledContainerTest >> classToTest [
	^ SpLabelledContainerPrestenter
]

{ #category : #running }
SpLabelledContainerTest >> initializeTestedInstance [
	presenter content: SpTestingComposablePresenter.
]

{ #category : #tests }
SpLabelledContainerTest >> testContent [
	presenter content: SpListPresenter.
	self assert: presenter subwidget class equals: SpListPresenter
]

{ #category : #tests }
SpLabelledContainerTest >> testDoesNotUnderstand [
	presenter content: SpListPresenter.
	presenter items: {'aaa' . 'bbb' . 'ccc'}.
	self assertCollection: presenter model items hasSameElements: {'aaa' . 'bbb' . 'ccc'}
]

{ #category : #tests }
SpLabelledContainerTest >> testLabelBottom [
	self openInstance: #labelBottom
]

{ #category : #tests }
SpLabelledContainerTest >> testLabelLeft [
	self openInstance:#labelLeft
]

{ #category : #tests }
SpLabelledContainerTest >> testLabelRight [
	self openInstance:#labelRight
]
