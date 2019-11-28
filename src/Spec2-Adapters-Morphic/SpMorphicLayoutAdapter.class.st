"
I'm a base adapter for layouts.
"
Class {
	#name : #SpMorphicLayoutAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Adapters-Morphic-Layout'
}

{ #category : #'as yet unclassified' }
SpMorphicLayoutAdapter >> adapter [
	
	^ nil
]

{ #category : #factory }
SpMorphicLayoutAdapter >> add: aPresenter constraints: constraints [
 
	self widgetDo: [ :aPanel |
		self 
			basicAdd: aPresenter 
			constraints: constraints 
			to: aPanel ]
]

{ #category : #adding }
SpMorphicLayoutAdapter >> addConstraits: constraints toChild: childMorph [

	self subclassResponsibility
]

{ #category : #initialization }
SpMorphicLayoutAdapter >> addKeyBindingsTo: aMorph [
	"This is called before the layout actually exists. 
	 Skipping it."
	
]

{ #category : #initialization }
SpMorphicLayoutAdapter >> applyStyle: aMorph [
	"layouts do not have style to apply"
]

{ #category : #factory }
SpMorphicLayoutAdapter >> applyStyle: aPresenter to: aMorph [
	self flag: #TODO. "This is a HACK, to apply styles just when I'm using new layouts 
	(otherwise, it interferes on how the old layout is displayed). 
	 Of course, this needs to be applied where it belongs, but for now we need to keep 
	 it this way :("
	
	aPresenter adapter ifNil: [ ^ self ].
	aPresenter adapter applyStyle: aMorph
]

{ #category : #private }
SpMorphicLayoutAdapter >> basicAdd: aPresenter constraints: constraints to: aPanel [ 

	self subclassResponsibility
]

{ #category : #private }
SpMorphicLayoutAdapter >> buildMorph: aPresenter constraints: constraints [ 
	| childMorph |

	childMorph := aPresenter buildWithSpec: constraints spec.
	self applyStyle: aPresenter to: childMorph.
	childMorph := self addConstraits: constraints toChild: childMorph.
	
	^ childMorph
]

{ #category : #factory }
SpMorphicLayoutAdapter >> buildWidget [

	^ nil
]

{ #category : #factory }
SpMorphicLayoutAdapter >> buildWithSpec [
	
	^ self widget
]

{ #category : #factory }
SpMorphicLayoutAdapter >> buildWithSpec: aString [ 

	^ self buildWithSpec
]

{ #category : #'as yet unclassified' }
SpMorphicLayoutAdapter >> layout: aLayout [

	"Basic apply visibility to the widget.
	This call is here because layouts and widgets work differently"
	self applyVisibility
]

{ #category : #factory }
SpMorphicLayoutAdapter >> newPanel [

	^ Morph new 
		changeTableLayout;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		color: Color transparent;
		borderColor: Color transparent;
		clipSubmorphs: true;
		beSticky "stop being grabbed";
		borderWidth: 0;
		extent: 0@0;
		yourself
]

{ #category : #protocol }
SpMorphicLayoutAdapter >> takeKeyboardFocus [

	self presenter presentersInFocusOrder 
		ifNotEmpty: [ :presenters | presenters first takeKeyboardFocus ]
]
