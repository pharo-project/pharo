"
A morph representing a ""page"" inside a `SpPaginatorMorph`.
Pages can be selected and users can control it with `SpPaginatorPageMorph>>#whenSelectedDo:`.
"
Class {
	#name : #SpPaginatorPageMorph,
	#superclass : #Morph,
	#instVars : [
		'selectedBlock',
		'model'
	],
	#category : #'Spec2-Adapters-Morphic-Paginator'
}

{ #category : #accessing }
SpPaginatorPageMorph class >> baseWidth [

	^ 14
]

{ #category : #accessing }
SpPaginatorPageMorph class >> defaultInset [

	^ 3
]

{ #category : #'event handling' }
SpPaginatorPageMorph >> handlesMouseDown: event [

	^ true
]

{ #category : #initialization }
SpPaginatorPageMorph >> initialize [

	super initialize.
	self 
		color: Color transparent;
		changeTableLayout;
		listDirection: #leftToRight;
		cellInset: self class defaultInset;
		cellSpacing: self class defaultInset;
		hResizing: #rigid;
		vResizing: #spaceFill;
		width: self class baseWidth.

	self addMorphBack: self newContentMorph.
]

{ #category : #menus }
SpPaginatorPageMorph >> model [

	^ model
]

{ #category : #menus }
SpPaginatorPageMorph >> model: anObject [

	model := anObject
]

{ #category : #'event handling' }
SpPaginatorPageMorph >> mouseDown: event [

	super mouseDown: event.
	selectedBlock ifNotNil: [ 
		selectedBlock value: self ]
]

{ #category : #initialization }
SpPaginatorPageMorph >> newContentMorph [

	^ Morph new
		color: Color white;
		borderColor: self theme borderColor;
		borderWidth: 1;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		yourself
]

{ #category : #events }
SpPaginatorPageMorph >> whenSelectedDo: aBlock [

	selectedBlock := aBlock
]
