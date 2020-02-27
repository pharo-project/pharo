"
I'm an adapter for Paned Layouts.
A paned layout will split (horizontally or vertically) a panel and allow its resize. 
"
Class {
	#name : #SpMorphicPanedAdapter,
	#superclass : #SpMorphicLayoutAdapter,
	#instVars : [
		'layout'
	],
	#category : #'Spec2-Adapters-Morphic-Layout'
}

{ #category : #private }
SpMorphicPanedAdapter >> addConstraits: constraints toChild: childMorph [
	"adds constraits by child"
	| theMorph |
	
	theMorph := childMorph.
	"keep constraints to layout usage"
	theMorph setProperty: #constraints toValue: constraints. 
	
	^ theMorph 
]

{ #category : #private }
SpMorphicPanedAdapter >> addSplitterIn: aPanel for: childMorph [

	aPanel submorphs size = 1 ifFalse: [ ^ self ].
	aPanel addMorphBack: self direction newSplitter
]

{ #category : #private }
SpMorphicPanedAdapter >> basicAdd: aPresenter constraints: constraints to: aPanel [
	| childMorph |

	childMorph := self buildMorph: aPresenter constraints: constraints.	
	aPanel addMorphBack: childMorph.
	self addSplitterIn: aPanel for: childMorph.
	
	^ childMorph
]

{ #category : #accessing }
SpMorphicPanedAdapter >> direction [

	^ self layout direction
]

{ #category : #accessing }
SpMorphicPanedAdapter >> layout [

	^ layout
]

{ #category : #factory }
SpMorphicPanedAdapter >> layout: aLayout [
	layout := aLayout.
	widget
		ifNil: [ widget := aLayout direction newWidgetOn: self.
			widget layoutPolicy position: aLayout position.
			self selector: self splitterNewSelector ].

	super layout: aLayout
]

{ #category : #factory }
SpMorphicPanedAdapter >> newHorizontal [

	^ self newPanel 
		listDirection: #leftToRight;
		yourself
]

{ #category : #factory }
SpMorphicPanedAdapter >> newPanel [

	^ super newPanel
		layoutPolicy: SpMorphicPanedLayout new;
		yourself
]

{ #category : #factory }
SpMorphicPanedAdapter >> newVertical [

	^ self newPanel 
		listDirection: #topToBottom;
		yourself	
]

{ #category : #private }
SpMorphicPanedAdapter >> resizingSetSelector [

	^ layout isHorizontal 
		ifTrue: [ #hResizing: ]
		ifFalse: [ #vResizing: ]
]

{ #category : #private }
SpMorphicPanedAdapter >> splitterNewSelector [

	^ layout isHorizontal 
		ifTrue: [ #newHorizontal ]
		ifFalse: [ #newVertical ]
]
