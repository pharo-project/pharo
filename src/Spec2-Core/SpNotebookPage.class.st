"
A page for a SpNotebookPresenter.
A page can have a title, an icon and some other interesting properties.

An important member is `SpNotebookPage>>#presenterProvider`, which contains a block to retrieve the content of the page, which can be resolved lazily.
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
		'#closeable',
		'#presenterProvider',
		'#retrievedBlock',
		'#activePresenter => WeakSlot'
	],
	#category : #'Spec2-Core-Widgets-Tab'
}

{ #category : #documentation }
SpNotebookPage class >> addDocumentExtraSections: aBuilder [
]

{ #category : #documentation }
SpNotebookPage class >> addDocumentSectionFactoryMethod: aBuilder [
	| selector |

	selector := self documentFactoryMethodSelector. 
	selector ifNil: [ ^ self ].
	 
	aBuilder newLine.
	aBuilder header: [ :builder | builder text: 'Factory method' ] withLevel: 2.
	aBuilder newLine.
	aBuilder text: 'You can use '. 
	aBuilder monospace: self name. 
	aBuilder text: ' in your presenters by sending '.
	aBuilder monospace: 'SpPresenter>>#', selector.
	aBuilder text: '.'
]

{ #category : #documentation }
SpNotebookPage class >> buildMicroDownUsing: aBuilder withComment: aString [
	
	super buildMicroDownUsing: aBuilder withComment: aString.
	"Factory method"
	self addDocumentSectionFactoryMethod: aBuilder.
	"Sections"
	self documentSections keysAndValuesDo: [ :label :methods |
		SpAbstractWidgetPresenter 
			addDocumentSection: aBuilder 
			label: label 
			methods: methods ]
]

{ #category : #documentation }
SpNotebookPage class >> documentFactoryMethodSelector [

	^ #newNotebookPage
]

{ #category : #documentation }
SpNotebookPage class >> documentSections [
		
	^ OrderedDictionary newFromPairs: {
		'API Methods'. self methods select: [ :each | each protocol = #'api' ].
		'Testing Methods'. self methods select: [ :each | each protocol = #'testing' ].
		'Events'. self methods select: [ :each | each protocol = #'api-events' ].
	 }
]

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

{ #category : #api }
SpNotebookPage >> activePresenter [
	"Answer current active presenter fot this page (it may be nil, if page has not been displayed yet)"
	
	^ activePresenter
]

{ #category : #api }
SpNotebookPage >> beCloseable [
	"Set the page as closeable, make a closing button to appear with the title."

	closeable := true
]

{ #category : #api }
SpNotebookPage >> icon [
	"Answer the icon (an instance of `Form`) to show with title"

	^ icon
]

{ #category : #api }
SpNotebookPage >> icon: anIcon [
	"Set the icon (an instance of `Form`) to show with title"

	icon := anIcon
]

{ #category : #initialization }
SpNotebookPage >> initialize [ 

	self class initializeSlots: self.
	super initialize.
	
	closeable := false.
	
	self property: #title whenChangedDo: [ self pageTitleChanged ].
	self property: #icon whenChangedDo: [ self pageTitleChanged ]
]

{ #category : #testing }
SpNotebookPage >> isCloseable [
	"Answer when the page is closeable or not"

	^ closeable
]

{ #category : #api }
SpNotebookPage >> layoutSpec [

	^ layoutSpec
]

{ #category : #api }
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

{ #category : #api }
SpNotebookPage >> presenterProvider [
	"Answer the presenter provider (tipically a block, but any valuable could work)."

	^ presenterProvider
]

{ #category : #api }
SpNotebookPage >> presenterProvider: aBlock [
	"Set the presenter provider (tipically a block, but any valuable could work).
	 The block will be executed before showing the page contents."

	presenterProvider := aBlock
]

{ #category : #api }
SpNotebookPage >> removeFromNotebook [
	"Remove this page from the notebook."

	owner removePage: self
]

{ #category : #api }
SpNotebookPage >> resetContent [
	"force a redraw of contents of this page (next time it is selected)"
	
	self owner updatePageContent: self
]

{ #category : #private }
SpNotebookPage >> retrievePresenter [
	"refresh all the time, because content may have change"

	activePresenter := self presenterProvider value.
	retrievedBlock ifNotNil: [ 
		retrievedBlock cull: activePresenter ].
	^ activePresenter
]

{ #category : #api }
SpNotebookPage >> title [
	"Answer the title to show in the notebook"

	^ title
]

{ #category : #api }
SpNotebookPage >> title: aString [
	"Set the title to show in the notebook"

	title := aString
]

{ #category : #'api-events' }
SpNotebookPage >> whenRetrievedDo: aBlock [ 
	"Inform when a page has been retrived, allowing extra settings.
	 `aBlock` is a block with one orgument (the presenter retrieved)"
	
	retrievedBlock := aBlock
]
