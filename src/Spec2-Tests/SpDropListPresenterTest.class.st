Class {
	#name : #SpDropListPresenterTest,
	#superclass : #SpSmokeTest,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #running }
SpDropListPresenterTest >> classToTest [
	^ SpDropListPresenter
]

{ #category : #tests }
SpDropListPresenterTest >> testSortingBlock [
	| count |
	count := 0.
	presenter
		whenSortingBlockChangedDo: [ :sortFunction | count := count + 1 ].
	presenter items: #(3 8 1 0).
	presenter sortingBlock: [ :a :b | a model < b model ].
	self assert: count equals: 1.
	self assert: (presenter model at: 1) model equals: 0
]
