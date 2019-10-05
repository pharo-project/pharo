Class {
	#name : #SpMorphicScrollableAdapter,
	#superclass : #SpMorphicLayoutAdapter,
	#instVars : [
		'layout'
	],
	#category : #'Spec2-Adapters-Morphic-Layout'
}

{ #category : #protocol }
SpMorphicScrollableAdapter >> addConstraits: constraints toChild: childMorph [
	"adds constraits by child"
	| theMorph |
	
	theMorph := childMorph.
	"keep constraints to layout usage"
	theMorph setProperty: #constraints toValue: constraints. 
	
	^ theMorph 
]

{ #category : #protocol }
SpMorphicScrollableAdapter >> basicAdd: aPresenter constraints: constraints to: aPanel [
	| childMorph |

	childMorph := self buildMorph: aPresenter constraints: constraints.	
	
	childMorph 
		hResizing: #spaceFill;
		vResizing: #spaceFill.

	aPanel scrollTarget: childMorph.
	
	^ childMorph
]

{ #category : #protocol }
SpMorphicScrollableAdapter >> layout: aLayout [
	
	widget ifNil: [ 
		widget := GeneralScrollPane new
			changeScrollerTableLayout;
			hResizing: #spaceFill;
			vResizing: #spaceFill;
			color: Color transparent;
			borderColor: Color transparent;
			yourself ].

	super layout: aLayout
]
