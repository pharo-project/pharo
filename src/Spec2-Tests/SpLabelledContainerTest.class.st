Class {
	#name : #SpLabelledContainerTest,
	#superclass : #SpSmokeTest,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #running }
SpLabelledContainerTest >> classToTest [
	^ SpLabelledContainer
]

{ #category : #running }
SpLabelledContainerTest >> initializeTestedInstance [
	presenter content: SpTestingPresenter
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
