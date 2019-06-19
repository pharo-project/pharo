"
Description
--------------------

I am a presenter used to display a link. 

My adaptor should display me as a link the user should be able to click and it will open a web page in the user browser.

If I have no label, I will show the link directly as a label.

Public API and Key Messages
--------------------

- #url: 		Used to set the URL of the page to show
- #label: 	Used to know which text to display in the page

Examples
--------------------
	
	(myPresenter instantiate: LinkPresenter)
		url: 'https://pharo.org';
		label: 'Pharo website'.
		
	myPresenter newLink
		url: 'https://pharo.org';
		label: 'Pharo website'.
		
	myPresenter newLink url: 'https://pharo.org'. ""The label here will be 'https://pharo.org'""

	self example
"
Class {
	#name : #LinkPresenter,
	#superclass : #AbstractWidgetPresenter,
	#instVars : [
		'urlHolder',
		'labelHolder'
	],
	#category : #'Spec-Core-Widgets'
}

{ #category : #specs }
LinkPresenter class >> adapterName [
	^ #LinkAdapter
]

{ #category : #specs }
LinkPresenter class >> title [
	^ 'Link presenter'
]

{ #category : #initialization }
LinkPresenter >> defaultColor [
	^ nil
]

{ #category : #initialization }
LinkPresenter >> initialize [

	super initialize.

	labelHolder := nil asValueHolder.
	urlHolder := nil asValueHolder.
	labelHolder whenChangedDo: [ self changed: #getText ]
]

{ #category : #api }
LinkPresenter >> label [
	^ labelHolder value ifNil: [ self url ]
]

{ #category : #api }
LinkPresenter >> label: aString [
	^ labelHolder value: aString
]

{ #category : #accessing }
LinkPresenter >> labelHolder [
	^ labelHolder
]

{ #category : #api }
LinkPresenter >> url [
	^ urlHolder value
]

{ #category : #api }
LinkPresenter >> url: aString [
	urlHolder value: aString
]

{ #category : #accessing }
LinkPresenter >> urlHolder [
	^ urlHolder
]
