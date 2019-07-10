Class {
	#name : #NotebookPresenterTest,
	#superclass : #SpecTestCase,
	#category : #'Spec-Tests-Core-Widgets'
}

{ #category : #running }
NotebookPresenterTest >> classToTest [
	^ NotebookPresenter
]

{ #category : #'as yet unclassified' }
NotebookPresenterTest >> mockPage [
	^ NotebookPage
		title: 'Mock'
		icon: (self iconNamed: #changeUpdate)
		provider: [ ButtonPresenter new ]
]

{ #category : #tests }
NotebookPresenterTest >> testAddPage [
	self assertEmpty: presenter pages.
	presenter addPage: self mockPage.
	self assert: presenter pages size equals: 1
	
]

{ #category : #tests }
NotebookPresenterTest >> testPageAt [
	| page |
	presenter addPage: self mockPage.
	page := NotebookPage title: 'test' provider: [ ButtonPresenter new ].
	presenter addPage: page.
	self assert: (presenter pageAt: 2) equals: page
]

{ #category : #tests }
NotebookPresenterTest >> testRemovePage [
	| page |
	presenter addPage: self mockPage.
	page := NotebookPage title: 'test' provider: [ ButtonPresenter new ].
	presenter addPage: page.
	self assert: presenter pages size equals: 2.
	presenter removePage: page.
	self assert: presenter pages size equals: 1
]

{ #category : #tests }
NotebookPresenterTest >> testRemovePageAt [
	| page |
	presenter addPage: self mockPage.
	page := NotebookPage title: 'test' provider: [ ButtonPresenter new ].
	presenter addPage: page.
	self assert: presenter pages size equals: 2.
	presenter removePageAt: 1.
	self assertCollection: presenter pages hasSameElements: {page}
]

{ #category : #tests }
NotebookPresenterTest >> testSelectPage [
	| mock mock2 |
	mock := self mockPage.
	mock2 := NotebookPage title: 'test' provider: [ ButtonPresenter new ].
	presenter addPage: mock.
	presenter addPage: mock2.
	presenter selectPage: mock2.
	self assert: presenter selectedPage equals: mock2
]

{ #category : #tests }
NotebookPresenterTest >> testSelectPageIndex [
	| mock mock2 |
	mock := self mockPage.
	mock2 := NotebookPage title: 'test' provider: [ ButtonPresenter new ].
	presenter addPage: mock.
	presenter addPage: mock2.
	presenter selectPageIndex: 2.
	self assert: presenter selectedPage equals: mock2
]

{ #category : #tests }
NotebookPresenterTest >> testWhenPagesChangedDo [
	| counter |
	counter := 0.
	self assertEmpty: presenter pages.
	presenter whenPagesChangedDo: [ counter := counter + 1 ].
	presenter addPage: self mockPage.
	self assert: counter equals: 1
]

{ #category : #tests }
NotebookPresenterTest >> testWhenSelectedPageChangedDo [
	| mock mock2 counter selected |
	counter := 0.
	mock := self mockPage.
	mock2 := NotebookPage title: 'test' provider: [ ButtonPresenter new ].
	presenter
		whenSelectedPageChangedDo: [ :page | 
			selected := page.
			counter := counter + 1 ].
	presenter addPage: mock.
	presenter addPage: mock2.
	presenter selectPageIndex: 2.
	self assert: counter equals: 1.
	self assert: selected equals: mock2
]
