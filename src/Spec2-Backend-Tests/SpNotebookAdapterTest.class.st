Class {
	#name : #SpNotebookAdapterTest,
	#superclass : #SpAbstractWidgetAdapterTest,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #accessing }
SpNotebookAdapterTest >> classToTest [
	^ SpNotebookPresenter
]

{ #category : #running }
SpNotebookAdapterTest >> initializeTestedInstance [
	presenter
		addPage: (SpNotebookPage title: 'Mock' icon: (self iconNamed: #changeUpdate) provider: [ SpButtonPresenter new ]);
		addPage: (SpNotebookPage title: 'Mock2' icon: (self iconNamed: #changeUpdate) provider: [ SpLabelPresenter new ])
]

{ #category : #tests }
SpNotebookAdapterTest >> testAddPage [
	self assert: self adapter numberOfTabs equals: 2.
	presenter addPage: (SpNotebookPage title: 'Mock3' provider: [ SpLabelPresenter new ]).
	self assert: self adapter numberOfTabs equals: 3
]

{ #category : #tests }
SpNotebookAdapterTest >> testClickOnPage [
	self adapter clickTab: 2.
	self assert: self adapter selectedPageName equals: 'Mock2'
]

{ #category : #tests }
SpNotebookAdapterTest >> testRemoveAll [

	self assert: self adapter numberOfTabs equals: 2.
	presenter removeAll.
	self assert: self adapter numberOfTabs equals: 0
]

{ #category : #tests }
SpNotebookAdapterTest >> testRemovePage [
	| page |
	presenter addPage: (page := SpNotebookPage title: 'Mock3' provider: [ SpLabelPresenter new ]).
	self assert: self adapter numberOfTabs equals: 3.
	presenter removePage: page.
	self assert: self adapter numberOfTabs equals: 2
]

{ #category : #tests }
SpNotebookAdapterTest >> testRemovePageAt [
	presenter addPage: (SpNotebookPage title: 'Mock3' provider: [ SpLabelPresenter new ]).
	self assert: self adapter numberOfTabs equals: 3.
	presenter removePageAt: 2.
	self assert: self adapter numberOfTabs equals: 2.
	self assert: self adapter selectedPageName equals: 'Mock'
]

{ #category : #tests }
SpNotebookAdapterTest >> testSelectedPage [
	presenter selectPageIndex: 2.
	self assert: self adapter selectedPageName equals: 'Mock2'
]
