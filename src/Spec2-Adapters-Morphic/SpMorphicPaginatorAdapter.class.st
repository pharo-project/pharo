Class {
	#name : #SpMorphicPaginatorAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#instVars : [
		'backendUpdating'
	],
	#category : #'Spec2-Adapters-Morphic-Paginator'
}

{ #category : #private }
SpMorphicPaginatorAdapter >> backendDetectCyclesDo: aBlock [ 

	self isBackendUpdating ifTrue: [ ^ self ].
	^ aBlock value
]

{ #category : #private }
SpMorphicPaginatorAdapter >> backendUpdatingDo: aBlock [ 
	| oldStatus |

	oldStatus := backendUpdating.
	backendUpdating := true.
	aBlock ensure: [ 
		backendUpdating := oldStatus ]
]

{ #category : #factory }
SpMorphicPaginatorAdapter >> buildWidget [
	| morph |
	
	morph := SpPaginatorMorph new
		addPages: self presenter pages;
		selectPage: self presenter pages size;
		yourself.

	self presenter whenSelectedPageChangedDo: [ :aNumber |
		self backendDetectCyclesDo: [ 
				morph selectPage: aNumber ] ].
	morph whenSelectedPageChangedDo: [ :aNumber |
		self backendUpdatingDo: [ 
			self presenter selectPage: aNumber ] ].
	
	self presenter whenVisiblePagesChangedDo: [ :aNumber |
		self backendDetectCyclesDo: [ 
			morph numberOfPagesShown: aNumber ] ].
	morph whenNumberOfPagesShownChangedDo: [ :aNumber |
		self backendUpdatingDo: [
			self presenter visiblePages: aNumber ] ].
	
	self presenter whenPageAddedDo: [ :anObject | 
		morph 
			addPage: anObject; 
			selectLastPage ].
	self presenter whenPageRemovedDo: [ :anObject |
		anObject 
			ifNotNil: [ morph removePage: anObject ]
			ifNil: [ morph removeAllPages ] ].
		
	morph selectionMorph whenDraggingDo: [ :delta | 
		self presenter dragging: (delta * 100 / widget width) ].

	^ morph
]

{ #category : #initialization }
SpMorphicPaginatorAdapter >> initialize [

	super initialize.
	backendUpdating := false
]

{ #category : #private }
SpMorphicPaginatorAdapter >> isBackendUpdating [

	^ backendUpdating
]

{ #category : #paginator }
SpMorphicPaginatorAdapter >> scrollByDeltaPercent: percent [
	| millerWidth millerDelta |
	
	millerWidth := self widget scroller localSubmorphBounds width.
	millerDelta := millerWidth * (percent / 100).
	self widget scroller offset: (self widget scroller offset x + millerDelta)@0
]
