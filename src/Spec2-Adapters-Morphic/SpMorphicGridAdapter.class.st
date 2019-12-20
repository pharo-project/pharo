"
I'm an adapter for Grid Layouts.
A grid layout will arrange components according to its coordinates and span information.
"
Class {
	#name : #SpMorphicGridAdapter,
	#superclass : #SpMorphicLayoutAdapter,
	#category : #'Spec2-Adapters-Morphic-Layout'
}

{ #category : #private }
SpMorphicGridAdapter >> addConstraits: constraints toChild: childMorph [
	"adds constraits by child"
	
	
	childMorph hResizing: #spaceFill.
	childMorph vResizing: #spaceFill.
	childMorph 
		setProperty: #gridConstraints 
		toValue: constraints.
	
	^ childMorph
]

{ #category : #private }
SpMorphicGridAdapter >> basicAdd: aPresenter constraints: constraints to: aPanel [
	| childMorph |

	childMorph := self 
		buildMorph: aPresenter 
		constraints: constraints.	
	aPanel addMorphBack: childMorph.
		
	^ childMorph	
]

{ #category : #factory }
SpMorphicGridAdapter >> layout: aLayout [
	"build of widget was differed up to here (to know what is the direction)"

	widget ifNil: [ 
		widget := self newPanel.
		widget layoutPolicy layout: aLayout ].

	super layout: aLayout
]

{ #category : #factory }
SpMorphicGridAdapter >> newPanel [

	^ super newPanel
		layoutPolicy: SpMorphicGridLayout new;
		yourself
]
