Class {
	#name : #NotebookAdapterTest,
	#superclass : #AbstractWidgetAdapterTestCase,
	#category : #'Spec-BackendTests'
}

{ #category : #accessing }
NotebookAdapterTest >> classToTest [
	^ NotebookPresenter
]

{ #category : #running }
NotebookAdapterTest >> initializeTestedInstance [
	presenter
		addPage: (NotebookPage title: 'Mock' icon: (self iconNamed: #changeUpdate) provider: [ ButtonPresenter new ]);
		addPage: (NotebookPage title: 'Mock2' icon: (self iconNamed: #changeUpdate) provider: [ LabelPresenter new ])
]

{ #category : #tests }
NotebookAdapterTest >> testAddPage [
	self assert: self adapter numberOfTabs equals: 2.
	presenter addPage: (NotebookPage title: 'Mock3' provider: [ LabelPresenter new ]).
	self assert: self adapter numberOfTabs equals: 3
]

{ #category : #tests }
NotebookAdapterTest >> testClickOnPage [
	self adapter clickTab: 2.
	self assert: self adapter selectedPageName equals: 'Mock2'
]

{ #category : #tests }
NotebookAdapterTest >> testRemovePage [
	| page |
	presenter addPage: (page := NotebookPage title: 'Mock3' provider: [ LabelPresenter new ]).
	self assert: self adapter numberOfTabs equals: 3.
	presenter removePage: page.
	self assert: self adapter numberOfTabs equals: 2
]

{ #category : #tests }
NotebookAdapterTest >> testRemovePageAt [
	presenter addPage: (NotebookPage title: 'Mock3' provider: [ LabelPresenter new ]).
	self assert: self adapter numberOfTabs equals: 3.
	presenter removePageAt: 2.
	self assert: self adapter numberOfTabs equals: 2.
	self assert: self adapter selectedPageName equals: 'Mock'
]

{ #category : #tests }
NotebookAdapterTest >> testSelectedPage [
	presenter selectPageIndex: 2.
	self assert: self adapter selectedPageName equals: 'Mock2'
]
