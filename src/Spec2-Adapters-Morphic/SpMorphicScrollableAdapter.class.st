"
I'm a morphic adapter for a scrollable layout. I contain a main widget that takes as much space as he wants and I add scrolling to it.

Internally, I create a GeneralScrollPane.
When I'm empty, I set the GeneralScrollPane's to an empty panel morph which I keep around to know if I'm empty or not.
"
Class {
	#name : #SpMorphicScrollableAdapter,
	#superclass : #SpMorphicLayoutAdapter,
	#instVars : [
		'layout',
		'emptyLayoutMarker'
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

{ #category : #accessing }
SpMorphicScrollableAdapter >> children [

	widget scrollTarget == emptyLayoutMarker
		ifTrue: [ ^ #() ].
	^ { widget scrollTarget }
]

{ #category : #protocol }
SpMorphicScrollableAdapter >> layout: aLayout [

	emptyLayoutMarker := PanelMorph new 
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		yourself.
	widget ifNil: [
		widget := GeneralScrollPane new
			changeScrollerTableLayout;
			hResizing: #spaceFill;
			vResizing: #spaceFill;
			color: Color transparent;
			borderColor: Color transparent;
			scrollTarget: emptyLayoutMarker;
			yourself ].

	aLayout child ifNotNil: [ 
		self 
			basicAdd: aLayout child
			constraints: (aLayout constraintsFor: aLayout child)
			to: widget ].

	aLayout whenChildrenAddedDo: [ :ann |
		self 
			basicAdd: ann child
			constraints: (aLayout constraintsFor: ann child) 
			to: widget ].
		
	aLayout whenChildrenRemovedDo: [ :ann |
		self removeChild: ann child ].

	super layout: aLayout
]

{ #category : #removing }
SpMorphicScrollableAdapter >> removeChild: aPresenter [

	self assert: aPresenter adapter widget == widget scrollTarget.
	widget scrollTarget: emptyLayoutMarker
]
