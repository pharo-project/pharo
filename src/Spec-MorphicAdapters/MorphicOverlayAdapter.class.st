Class {
	#name : #MorphicOverlayAdapter,
	#superclass : #MorphicLayoutAdapter,
	#instVars : [
		'layout'
	],
	#category : #'Spec-MorphicAdapters-Layout'
}

{ #category : #protocol }
MorphicOverlayAdapter >> addConstraits: constraints toChild: childMorph [
	"adds constraits by child"
	| theMorph |
	
	theMorph := childMorph.
	"keep constraints to layout usage"
	theMorph setProperty: #constraints toValue: constraints. 
	
	^ theMorph 
]

{ #category : #protocol }
MorphicOverlayAdapter >> basicAdd: aPresenter constraints: constraints to: aWidget [
	| childMorph |

	childMorph := self buildMorph: aPresenter constraints: constraints.	
	aWidget addMorph: childMorph.
	^ childMorph
]

{ #category : #protocol }
MorphicOverlayAdapter >> layout: aLayout [
	widget
		ifNil: [ widget := PanelMorph new
				layoutPolicy: MorphicOverlayLayout new;
				hResizing: #spaceFill;
				vResizing: #spaceFill;
				color: Color transparent;
				borderColor: Color transparent;
				yourself ].

	super layout: aLayout
]
