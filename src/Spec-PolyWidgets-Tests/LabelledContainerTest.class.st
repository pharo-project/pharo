Class {
	#name : #LabelledContainerTest,
	#superclass : #SpecSmokeTestCase,
	#category : #'Spec-PolyWidgets-Tests'
}

{ #category : #running }
LabelledContainerTest >> classToTest [
	^ LabelledContainer
]

{ #category : #running }
LabelledContainerTest >> initializeTestedInstance [
	presenter content: TestingComposablePresenter.
]

{ #category : #tests }
LabelledContainerTest >> testContent [
	presenter content: ListPresenter.
	self assert: presenter subwidget class equals: ListPresenter
]

{ #category : #tests }
LabelledContainerTest >> testDoesNotUnderstand [
	presenter content: ListPresenter.
	presenter items: {'aaa' . 'bbb' . 'ccc'}.
	self assertCollection: presenter model items hasSameElements: {'aaa' . 'bbb' . 'ccc'}
]

{ #category : #tests }
LabelledContainerTest >> testLabelBottom [
	self openInstance: #labelBottom
]

{ #category : #tests }
LabelledContainerTest >> testLabelLeft [
	self openInstance:#labelLeft
]

{ #category : #tests }
LabelledContainerTest >> testLabelRight [
	self openInstance:#labelRight
]
