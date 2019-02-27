"
I'm an adapter for Grid Layouts.
A grid layout will arrange components according to its coordinates and span information.
"
Class {
	#name : #MorphicGridAdapter,
	#superclass : #MorphicLayoutAdapter,
	#category : #'Spec-MorphicAdapters-Layout'
}

{ #category : #private }
MorphicGridAdapter >> addConstraits: constraints toChild: childMorph [
	"adds constraits by child"
	
	
	childMorph hResizing: #spaceFill.
	childMorph vResizing: #spaceFill.
	childMorph 
		setProperty: #gridConstraints 
		toValue: constraints.
	
	^ childMorph
]

{ #category : #private }
MorphicGridAdapter >> basicAdd: aPresenter constraints: constraints to: aPanel [
	| childMorph |

	childMorph := self 
		buildMorph: aPresenter 
		constraints: constraints.	
	aPanel addMorphBack: childMorph.
		
	^ childMorph	
]

{ #category : #factory }
MorphicGridAdapter >> layout: aLayout [

	"build of widget was differed up to here (to know what is the direction)"
	widget := self newPanel.
	widget layoutPolicy
		padding: aLayout borderWidth;
		rowSpacing: aLayout rowSpacing;
		columnSpacing: aLayout columnSpacing;
		rowHomogeneous: aLayout isRowHomogeneous;
		columnHomogeneous: aLayout isColumnHomogeneous
]

{ #category : #factory }
MorphicGridAdapter >> newPanel [

	^ PanelMorph new 
		layoutPolicy: GridLayout new;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		yourself
]
