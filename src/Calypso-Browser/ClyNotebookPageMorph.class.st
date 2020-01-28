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
		'closable',
		'label',
		'actions'
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

{ #category : #accessing }
ClyNotebookPageMorph >> backgroundColor [

	^ Color transparent
]

{ #category : #accessing }
ClyNotebookPageMorph >> beClosable [

	closable := true.
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
	closable := false.
	labelPresenter := ClyNotebookPageHeaderPresenter on: self.
	self flag: #TODO. "I'm hardcoding this width because I need to keep this sizes consistent. 
	But this is not good, think a better way."
	labelMorph := (labelPresenter buildWithSpec
		hResizing: #rigid;
		width: 120;
		yourself)
]

{ #category : #testing }
ClyNotebookPageMorph >> isClosable [

	^ closable
]

{ #category : #accessing }
ClyNotebookPageMorph >> label [

	^ label
]

{ #category : #accessing }
ClyNotebookPageMorph >> label: aString [

	label := aString
]

{ #category : #accessing }
ClyNotebookPageMorph >> markDirty: aBoolean [

	labelPresenter markDirty: aBoolean
]
