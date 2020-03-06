"
I'm a morphic adapter for an overlay layout. I contain a main widget and an overlay that superposes it.

Internally, I create a normal panel morph with a SpMorphicOverlayLayout.
The SpMorphicOverlayLayout collaborates with the layout's constraints to decide wether a child is the main widget or an overlay widget.
"
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
	aWidget addMorphBack: childMorph.
	^ childMorph
]

{ #category : #accessing }
SpMorphicOverlayAdapter >> children [
	
	^ widget submorphs
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

	aLayout whenChildrenAddedDo: [ :ann |
		self 
			basicAdd: ann child
			constraints: (aLayout constraintsFor: ann child) 
			to: widget ].
		
	aLayout whenChildrenRemovedDo: [ :ann |
		self removeChild: ann child ].

	super layout: aLayout
]

{ #category : #accessing }
SpMorphicOverlayAdapter >> removeChild: aPresenter [
	
	| morph |
	morph := aPresenter adapter widget.
	widget removeMorph: morph
]
