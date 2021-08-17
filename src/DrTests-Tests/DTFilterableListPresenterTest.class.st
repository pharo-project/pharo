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

{ #category : #tests }
DTFilterableListPresenterTest >> testFilterStrings [
	filterableList filterTextInput text: 'foo|bar'.
	
	self assertCollection: filterableList filterStrings hasSameElements: #('foo*' 'bar*').
	
	filterableList filterTextInput text: '|bar'.
	
	self assertCollection: filterableList filterStrings hasSameElements: #('bar*').
	
	filterableList filterTextInput text: 'foo|'.
	
	self assertCollection: filterableList filterStrings hasSameElements: #('foo*').
]

{ #category : #tests }
DTFilterableListPresenterTest >> testFilterWorks [
	self assertCollection: filterableList allItems equals: (1 to: 20).
	self assertCollection: filterableList visibleItems equals: (1 to: 20).
	
	filterableList filterTextInput text: '2'.
	
	self assertCollection: filterableList allItems equals: (1 to: 20).
	self assertCollection: filterableList visibleItems equals: #(2 20).
	
	filterableList filterTextInput text: ''.
	
	self assertCollection: filterableList allItems equals: filterableList visibleItems. "We want to show everything if no filter is written."
]

{ #category : #running }
DTFilterableListPresenterTest >> testLabel [
	self assert: filterableList label equals: ''.
	
	filterableList label: 'title'.
	
	self assert: filterableList label equals: 'title'.
]
