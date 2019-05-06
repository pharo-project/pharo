Class {
	#name : #MorphicScrollableAdapter,
	#superclass : #MorphicLayoutAdapter,
	#instVars : [
		'layout'
	],
	#category : #'Spec-MorphicAdapters-Layout'
}

{ #category : #protocol }
MorphicScrollableAdapter >> addConstraits: constraints toChild: childMorph [
	"adds constraits by child"
	| theMorph |
	
	theMorph := childMorph.
	"keep constraints to layout usage"
	theMorph setProperty: #constraints toValue: constraints. 
	
	^ theMorph 
]

{ #category : #protocol }
MorphicScrollableAdapter >> basicAdd: aPresenter constraints: constraints to: aPanel [
	| childMorph |

	childMorph := self buildMorph: aPresenter constraints: constraints.	
	
	childMorph 
		hResizing: #spaceFill;
		vResizing: #spaceFill.

	aPanel addMorph: childMorph.
	
	^ childMorph
]

{ #category : #protocol }
MorphicScrollableAdapter >> layout: aLayout [
	widget
		ifNil: [ widget := PanelMorph new
				changeTableLayout;
				hResizing: #spaceFill;
				vResizing: #spaceFill;
				color: Color transparent;
				borderColor: Color transparent;
				yourself ].

	super layout: aLayout
]
