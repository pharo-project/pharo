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
	#name : #SpLinkPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'#label => SpObservableSlot',
		'#action => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpLinkPresenter class >> adapterName [
	^ #LinkAdapter
]

{ #category : #api }
SpLinkPresenter >> action [
	^ action
]

{ #category : #api }
SpLinkPresenter >> action: aBlock [
	action := aBlock
]

{ #category : #initialization }
SpLinkPresenter >> defaultColor [
	^ nil
]

{ #category : #initialization }
SpLinkPresenter >> initialize [
	super initialize.

	self whenLabelChangedDo: [ self changed: #getText ]
]

{ #category : #api }
SpLinkPresenter >> label [
	^ label
]

{ #category : #api }
SpLinkPresenter >> label: aString [
	^ label := aString
]

{ #category : #api }
SpLinkPresenter >> url: aString [
	self action: [ WebBrowser openOn: aString ].
	self label ifNil: [ self label: aString ]
]

{ #category : #enumerating }
SpLinkPresenter >> whenActionChangedDo: aBlock [
	self property: #action whenChangedDo: aBlock
]

{ #category : #enumerating }
SpLinkPresenter >> whenLabelChangedDo: aBlock [
	self property: #label whenChangedDo: aBlock
]

{ #category : #enumerating }
SpLinkPresenter >> whenUrlChangedDo: aBlock [
	self property: #url whenChangedDo: aBlock
]
