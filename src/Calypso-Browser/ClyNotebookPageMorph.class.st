"
I am a page on a calypso notebook.
I define a different kind of header (icon, label, close) plus some actions on it.
"
Class {
	#name : #ClyNotebookPageMorph,
	#superclass : #SpNotebookPageMorph,
	#instVars : [
		'title',
		'icon',
		'labelPresenter',
		'label',
		'actions',
		'closeable'
	],
	#category : #'Calypso-Browser-Tabs'
}

{ #category : #'instance creation' }
ClyNotebookPageMorph class >> newLabel: aString icon: anIcon provider: aBlock [

	^ self new 
		label: aString; 
		icon: anIcon;
		lazyPageMorphCreation: aBlock;
		yourself
]

{ #category : #accessing }
ClyNotebookPageMorph >> actions [

	^ actions
]

{ #category : #accessing }
ClyNotebookPageMorph >> actions: aCollection [

	actions := aCollection
]

{ #category : #'adding-removing' }
ClyNotebookPageMorph >> addToSelection [
	
	owner addToSelection: self
]

{ #category : #accessing }
ClyNotebookPageMorph >> backgroundColor [

	^ Color transparent
]

{ #category : #accessing }
ClyNotebookPageMorph >> beCloseable [

	closeable := true.
	labelPresenter updatePresenter
]

{ #category : #accessing }
ClyNotebookPageMorph >> icon [

	^ icon
]

{ #category : #accessing }
ClyNotebookPageMorph >> icon: anIcon [

	icon := anIcon.
	labelPresenter updatePresenter
]

{ #category : #initialization }
ClyNotebookPageMorph >> initialize [

	super initialize.
	closeable := false.
	labelPresenter := ClyNotebookPageHeaderPresenter on: self.
	self flag: #TODO. "I'm hardcoding this width because I need to keep this sizes consistent. 
	But this is not good, think a better way."
	labelMorph := (labelPresenter buildWithSpec
		hResizing: #rigid;
		width: 120;
		yourself)
]

{ #category : #testing }
ClyNotebookPageMorph >> isCloseable [

	^ closeable
]

{ #category : #accessing }
ClyNotebookPageMorph >> label [

	^ label
]

{ #category : #accessing }
ClyNotebookPageMorph >> label: aStringOrText [

	label := aStringOrText
]

{ #category : #accessing }
ClyNotebookPageMorph >> markDirty: dirty [

	dirty
		ifTrue: [ self label: '*', self label ]
		ifFalse: [ 
			(self label beginsWith: '*')
				ifTrue: [  self label: self label allButFirst ] ].
	labelPresenter updatePresenter
]

{ #category : #'events handling' }
ClyNotebookPageMorph >> onClose: aBlock [

	labelPresenter onClose: aBlock.
	self beCloseable
]

{ #category : #'events handling' }
ClyNotebookPageMorph >> onDoubleClick: aBlock [

	labelPresenter onDoubleClick: aBlock
]

{ #category : #'adding-removing' }
ClyNotebookPageMorph >> removeFromSelection [
	owner removeFromSelection: self
]

{ #category : #accessing }
ClyNotebookPageMorph >> themeChanged [

	super themeChanged.
	self actions do: [ :each | each themeChanged ]
]
