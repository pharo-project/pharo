Class {
	#name : #NotebookPageMorph,
	#superclass : #Morph,
	#instVars : [
		'actualPageMorph',
		'lazyPageMorphCreation',
		'labelMorph',
		'model'
	],
	#category : #'Spec-MorphicAdapters-Notebook'
}

{ #category : #accessing }
NotebookPageMorph >> actualPageMorph [

	^ actualPageMorph ifNil: [
		actualPageMorph := self lazyPageMorphCreation value
			hResizing: #spaceFill;
			vResizing: #spaceFill;
			yourself ]
]

{ #category : #accessing }
NotebookPageMorph >> actualPageMorph: anObject [

	actualPageMorph := anObject
]

{ #category : #testing }
NotebookPageMorph >> isRendered [

	^ actualPageMorph notNil
]

{ #category : #accessing }
NotebookPageMorph >> labelMorph [

	^ labelMorph
]

{ #category : #accessing }
NotebookPageMorph >> labelMorph: anObject [

	labelMorph := anObject
]

{ #category : #accessing }
NotebookPageMorph >> lazyPageMorphCreation [

	^ lazyPageMorphCreation
]

{ #category : #accessing }
NotebookPageMorph >> lazyPageMorphCreation: anObject [

	lazyPageMorphCreation := anObject
]

{ #category : #accessing }
NotebookPageMorph >> model [

	^ model
]

{ #category : #accessing }
NotebookPageMorph >> model: anObject [

	model := anObject
]

{ #category : #accessing }
NotebookPageMorph >> reset [

	self actualPageMorph: nil
]
