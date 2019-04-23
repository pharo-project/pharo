"
I'm an adapter for Paned Layouts.
A paned layout will split (horizontally or vertically) a panel and allow its resize. 
"
Class {
	#name : #MorphicPanedAdapter,
	#superclass : #MorphicLayoutAdapter,
	#instVars : [
		'layout'
	],
	#category : #'Spec-MorphicAdapters-Layout'
}

{ #category : #private }
MorphicPanedAdapter >> addConstraits: constraints toChild: childMorph [
	"adds constraits by child"
	| theMorph |
	
	theMorph := childMorph.
	"keep constraints to layout usage"
	theMorph setProperty: #constraints toValue: constraints. 
	
	^ theMorph 
]

{ #category : #private }
MorphicPanedAdapter >> addSplitterIn: aPanel for: childMorph [
	
	aPanel submorphs size = 1 ifFalse: [ ^ self ].
	aPanel addMorphBack: (PanedResizerMorph perform: self selector)
]

{ #category : #private }
MorphicPanedAdapter >> basicAdd: aPresenter constraints: constraints to: aPanel [
	| childMorph |

	childMorph := self buildMorph: aPresenter constraints: constraints.	
	aPanel addMorphBack: childMorph.
	self addSplitterIn: aPanel for: childMorph.
	
	^ childMorph
]

{ #category : #accessing }
MorphicPanedAdapter >> direction [

	^ self layout direction
]

{ #category : #accessing }
MorphicPanedAdapter >> layout [

	^ layout
]

{ #category : #factory }
MorphicPanedAdapter >> layout: aLayout [
	layout := aLayout.
	widget
		ifNil: [ widget := aLayout direction newWidgetOn: self.
			widget layoutPolicy position: aLayout position.
			self selector: self splitterNewSelector ].

	super layout: aLayout
]

{ #category : #factory }
MorphicPanedAdapter >> newHorizontal [

	^ self newPanel 
		listDirection: #leftToRight;
		yourself
]

{ #category : #factory }
MorphicPanedAdapter >> newPanel [

	^ PanelMorph new 
		layoutPolicy: PanedLayout new;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		color: Color transparent;
		yourself
]

{ #category : #factory }
MorphicPanedAdapter >> newVertical [

	^ self newPanel 
		listDirection: #topToBottom;
		yourself	
]

{ #category : #private }
MorphicPanedAdapter >> resizingSetSelector [

	^ layout isHorizontal 
		ifTrue: [ #hResizing: ]
		ifFalse: [ #vResizing: ]
]

{ #category : #private }
MorphicPanedAdapter >> splitterNewSelector [

	^ layout isHorizontal 
		ifTrue: [ #newHorizontal ]
		ifFalse: [ #newVertical ]
]
