Class {
	#name : #SpMorphicOverlayAdapter,
	#superclass : #SpMorphicLayoutAdapter,
	#instVars : [
		'layout'
	],
	#category : #'Spec2-Adapters-Morphic-Layout'
}

{ #category : #protocol }
SpMorphicOverlayAdapter >> addConstraits: constraints toChild: childMorph [
	"adds constraits by child"
	| theMorph |
	
	theMorph := childMorph.
	"keep constraints to layout usage"
	theMorph setProperty: #constraints toValue: constraints. 
	
	^ theMorph 
]

{ #category : #protocol }
SpMorphicOverlayAdapter >> basicAdd: aPresenter constraints: constraints to: aWidget [
	| childMorph |

	childMorph := self buildMorph: aPresenter constraints: constraints.	
	aWidget addMorph: childMorph.
	^ childMorph
]

{ #category : #protocol }
SpMorphicOverlayAdapter >> layout: aLayout [
	widget
		ifNil: [ widget := PanelMorph new
				layoutPolicy: SpMorphicOverlayLayout new;
				hResizing: #spaceFill;
				vResizing: #spaceFill;
				color: Color transparent;
				borderColor: Color transparent;
				yourself ].

	super layout: aLayout
]
