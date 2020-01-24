Class {
	#name : #ClyNotebookPageMorph,
	#superclass : #SpNotebookPageMorph,
	#instVars : [
		'title',
		'icon',
		'labelPresenter',
		'closable',
		'label'
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

{ #category : #'as yet unclassified' }
ClyNotebookPageMorph >> backgroundColor [

	^ Color transparent
]

{ #category : #'as yet unclassified' }
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
