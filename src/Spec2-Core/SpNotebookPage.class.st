"
I represent a page for a NotebookPresenter.
I have title, icon and some properties.

In particular, I contain a presenterProvider, a block who's responsibility is to retrieve a presenter when demanded (lazily).
"
Class {
	#name : #SpNotebookPage,
	#superclass : #Object,
	#traits : 'SpTObservable',
	#classTraits : 'SpTObservable classTrait',
	#instVars : [
		'#owner',
		'#title => SpObservableSlot',
		'#icon => SpObservableSlot',
		'#layoutSpec => SpObservableSlot',
		'#presenterProvider',
		'#retrievedBlock',
		'#activePresenter => WeakSlot'
	],
	#category : #'Spec2-Core-Widgets-Tab'
}

{ #category : #'instance creation' }
SpNotebookPage class >> title: aString icon: anIcon provider: aBlock [

	^ self new 
		title: aString;
		icon: anIcon;
		presenterProvider: aBlock;
		yourself
]

{ #category : #'instance creation' }
SpNotebookPage class >> title: aString icon: anIcon provider: aBlock layoutSpec: aSymbol [

	^ self new 
		title: aString;
		icon: anIcon;
		presenterProvider: aBlock;
		layoutSpec: aSymbol;
		yourself
]

{ #category : #'instance creation' }
SpNotebookPage class >> title: aString provider: aBlock [

	^ self new 
		title: aString;
		presenterProvider: aBlock;
		yourself
]

{ #category : #accessing }
SpNotebookPage >> activePresenter [
	"This is the current active presenter fot this page"
	
	^ activePresenter
]

{ #category : #accessing }
SpNotebookPage >> icon [
	^ icon
]

{ #category : #accessing }
SpNotebookPage >> icon: anIcon [
	icon := anIcon
]

{ #category : #initialization }
SpNotebookPage >> initialize [ 

	self class initializeSlots: self.
	super initialize.
	
	self property: #title whenChangedDo: [ self pageTitleChanged ].
	self property: #icon whenChangedDo: [ self pageTitleChanged ]
]

{ #category : #accessing }
SpNotebookPage >> layoutSpec [

	^ layoutSpec
]

{ #category : #accessing }
SpNotebookPage >> layoutSpec: aSymbol [

	layoutSpec := aSymbol
]

{ #category : #accessing }
SpNotebookPage >> owner [

	^ owner
]

{ #category : #accessing }
SpNotebookPage >> owner: anObject [

	owner := anObject
]

{ #category : #private }
SpNotebookPage >> pageTitleChanged [

	self owner ifNil: [ ^ self ].
	self owner pageTitleChanged: self
]

{ #category : #accessing }
SpNotebookPage >> presenterProvider [

	^ presenterProvider
]

{ #category : #accessing }
SpNotebookPage >> presenterProvider: aBlock [

	presenterProvider := aBlock
]

{ #category : #accessing }
SpNotebookPage >> resetContent [
	"force a redraw of contents of this page (next time it is selected)"
	
	self owner updatePageContent: self
]

{ #category : #accessing }
SpNotebookPage >> retrievePresenter [
	"refresh all the time, because content may have change"

	activePresenter := self presenterProvider value.
	retrievedBlock ifNotNil: [ 
		retrievedBlock cull: activePresenter ].
	^ activePresenter
]

{ #category : #accessing }
SpNotebookPage >> title [

	^ title
]

{ #category : #accessing }
SpNotebookPage >> title: aString [

	title := aString
]

{ #category : #events }
SpNotebookPage >> whenRetrievedDo: aBlock [ 

	retrievedBlock := aBlock
]
