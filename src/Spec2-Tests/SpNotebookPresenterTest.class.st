Class {
	#name : #SpNotebookPresenterTest,
	#superclass : #SpTest,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #running }
SpNotebookPresenterTest >> classToTest [
	^ SpNotebookPresenter
]

{ #category : #'as yet unclassified' }
SpNotebookPresenterTest >> mockPage [
	^ SpNotebookPage
		title: 'Mock'
		icon: (self iconNamed: #changeUpdate)
		provider: [ SpButtonPresenter new ]
]

{ #category : #tests }
SpNotebookPresenterTest >> testAddPage [
	self assertEmpty: presenter pages.
	presenter addPage: self mockPage.
	self assert: presenter pages size equals: 1
	
]

{ #category : #tests }
SpNotebookPresenterTest >> testPageAt [
	| page |
	presenter addPage: self mockPage.
	page := SpNotebookPage title: 'test' provider: [ SpButtonPresenter new ].
	presenter addPage: page.
	self assert: (presenter pageAt: 2) equals: page
]

{ #category : #tests }
SpNotebookPresenterTest >> testRemovePage [
	| page |
	presenter addPage: self mockPage.
	page := SpNotebookPage title: 'test' provider: [ SpButtonPresenter new ].
	presenter addPage: page.
	self assert: presenter pages size equals: 2.
	presenter removePage: page.
	self assert: presenter pages size equals: 1
]

{ #category : #tests }
SpNotebookPresenterTest >> testRemovePageAt [
	| page |
	presenter addPage: self mockPage.
	page := SpNotebookPage title: 'test' provider: [ SpButtonPresenter new ].
	presenter addPage: page.
	self assert: presenter pages size equals: 2.
	presenter removePageAt: 1.
	self assertCollection: presenter pages hasSameElements: {page}
]

{ #category : #tests }
SpNotebookPresenterTest >> testSelectPage [
	| mock mock2 |
	mock := self mockPage.
	mock2 := SpNotebookPage title: 'test' provider: [ SpButtonPresenter new ].
	presenter addPage: mock.
	presenter addPage: mock2.
	presenter selectPage: mock2.
	self assert: presenter selectedPage equals: mock2
]

{ #category : #tests }
SpNotebookPresenterTest >> testSelectPageIndex [
	| mock mock2 |
	mock := self mockPage.
	mock2 := SpNotebookPage title: 'test' provider: [ SpButtonPresenter new ].
	presenter addPage: mock.
	presenter addPage: mock2.
	presenter selectPageIndex: 2.
	self assert: presenter selectedPage equals: mock2
]

{ #category : #tests }
SpNotebookPresenterTest >> testWhenPagesChangedDo [
	| counter |
	counter := 0.
	self assertEmpty: presenter pages.
	presenter whenPagesChangedDo: [ counter := counter + 1 ].
	presenter addPage: self mockPage.
	self assert: counter equals: 1
]

{ #category : #tests }
SpNotebookPresenterTest >> testWhenSelectedPageChangedDo [
	| mock mock2 counter selected |
	counter := 0.
	mock := self mockPage.
	mock2 := SpNotebookPage title: 'test' provider: [ SpButtonPresenter new ].
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
