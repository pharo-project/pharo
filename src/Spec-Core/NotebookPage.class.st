"
I represent a page for a NotebookPresenter.
I have title, icon and some properties.

In particular, I contain a presenterProvider, a block who's responsibility is to retrieve a presenter when demanded (lazily).
"
Class {
	#name : #NotebookPage,
	#superclass : #Object,
	#instVars : [
		'owner',
		'presenterProvider',
		'titleHolder',
		'iconHolder',
		'layoutSpecHolder'
	],
	#category : #'Spec-Core-Widgets-Tab'
}

{ #category : #'instance creation' }
NotebookPage class >> title: aString icon: anIcon provider: aBlock [

	^ self new 
		title: aString;
		icon: anIcon;
		presenterProvider: aBlock;
		yourself
]

{ #category : #'instance creation' }
NotebookPage class >> title: aString icon: anIcon provider: aBlock layoutSpec: aSymbol [

	^ self new 
		title: aString;
		icon: anIcon;
		presenterProvider: aBlock;
		layoutSpec: aSymbol;
		yourself
]

{ #category : #'instance creation' }
NotebookPage class >> title: aString provider: aBlock [

	^ self new 
		title: aString;
		presenterProvider: aBlock;
		yourself
]

{ #category : #accessing }
NotebookPage >> icon [

	^ iconHolder value
]

{ #category : #accessing }
NotebookPage >> icon: anIcon [

	iconHolder value: anIcon
]

{ #category : #initialization }
NotebookPage >> initialize [ 

	super initialize.
	titleHolder := nil asValueHolder.
	iconHolder := nil asValueHolder.
	layoutSpecHolder := nil asValueHolder.
	
	titleHolder whenChangedDo: [ self pageTitleChanged ].
	iconHolder whenChangedDo: [ self pageTitleChanged ]
]

{ #category : #accessing }
NotebookPage >> layoutSpec [
	^ layoutSpecHolder value
]

{ #category : #accessing }
NotebookPage >> layoutSpec: aSymbol [
	layoutSpecHolder value: aSymbol
]

{ #category : #accessing }
NotebookPage >> owner [

	^ owner
]

{ #category : #accessing }
NotebookPage >> owner: anObject [

	owner := anObject
]

{ #category : #private }
NotebookPage >> pageTitleChanged [

	self owner ifNil: [ ^ self ].
	self owner pageTitleChanged: self
]

{ #category : #accessing }
NotebookPage >> presenterProvider [

	^ presenterProvider
]

{ #category : #accessing }
NotebookPage >> presenterProvider: aBlock [

	presenterProvider := aBlock
]

{ #category : #accessing }
NotebookPage >> resetContent [
	"force a redraw of contents of this page (next time it is selected)"
	
	self owner updatePageContent: self
]

{ #category : #accessing }
NotebookPage >> retrievePresenter [

	^ self presenterProvider value
]

{ #category : #accessing }
NotebookPage >> title [

	^ titleHolder value
]

{ #category : #accessing }
NotebookPage >> title: anObject [

	titleHolder value: anObject
]
