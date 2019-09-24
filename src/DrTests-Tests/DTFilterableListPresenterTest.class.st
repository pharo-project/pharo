"
A DTFilterableListPresenterTest is a test class for testing the behavior of DTFilterableListPresenter
"
Class {
	#name : #DTFilterableListPresenterTest,
	#superclass : #TestCase,
	#instVars : [
		'filterableList'
	],
	#category : #'DrTests-Tests'
}

{ #category : #running }
DTFilterableListPresenterTest >> setUp [
	super setUp.
	filterableList := DTFilterableListPresenter new
							items: (1 to: 20);
							displayBlock: #asString;
							yourself.	
]

{ #category : #test }
DTFilterableListPresenterTest >> testFilterString [
	filterableList filterTextInput text: 'foo'.
	
	self assert: filterableList filterString equals: '*foo*'
]

{ #category : #tests }
DTFilterableListPresenterTest >> testFilterWorks [
	self assertCollection: filterableList allItems equals: (1 to: 20).
	self assertCollection: filterableList visibleItems equals: (1 to: 20).
	
	filterableList filterTextInput text: '2'.
	
	self assertCollection: filterableList allItems equals: (1 to: 20).
	self assertCollection: filterableList visibleItems equals: #(2 12 20).
]

{ #category : #running }
DTFilterableListPresenterTest >> testLabel [
	self assert: filterableList label equals: ''.
	
	filterableList label: 'title'.
	
	self assert: filterableList label equals: 'title'.
]
