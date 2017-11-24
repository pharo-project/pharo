Class {
	#name : #LazyTabPage,
	#superclass : #BorderedMorph,
	#instVars : [
		'actualPageMorph',
		'labelMorph',
		'lazyPageMorphCreation',
		'toolbar'
	],
	#category : #'Glamour-Morphic-Widgets'
}

{ #category : #accessing }
LazyTabPage >> actualPageMorph [
	^ actualPageMorph ifNil: 
		[actualPageMorph := 
			self lazyPageMorphCreation value
				hResizing: #spaceFill;
				vResizing: #spaceFill;
				yourself]
]

{ #category : #accessing }
LazyTabPage >> actualPageMorph: anObject [
	actualPageMorph := anObject
]

{ #category : #testing }
LazyTabPage >> isRendered [
	^ actualPageMorph notNil
]

{ #category : #accessing }
LazyTabPage >> labelMorph [
	^ labelMorph
]

{ #category : #accessing }
LazyTabPage >> labelMorph: anObject [
	labelMorph := anObject
]

{ #category : #accessing }
LazyTabPage >> lazyPageMorphCreation [
	^ lazyPageMorphCreation
]

{ #category : #accessing }
LazyTabPage >> lazyPageMorphCreation: anObject [
	lazyPageMorphCreation := anObject
]

{ #category : #accessing }
LazyTabPage >> toolbar [
	^ toolbar
]

{ #category : #accessing }
LazyTabPage >> toolbar: anObject [
	toolbar := anObject
]
