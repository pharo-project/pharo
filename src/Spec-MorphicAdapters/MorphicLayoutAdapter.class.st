"
I'm a base adapter for layouts.
"
Class {
	#name : #MorphicLayoutAdapter,
	#superclass : #AbstractMorphicAdapter,
	#category : #'Spec-MorphicAdapters-Layout'
}

{ #category : #'as yet unclassified' }
MorphicLayoutAdapter >> adapter [
	
	^ nil
]

{ #category : #factory }
MorphicLayoutAdapter >> add: aPresenter constraints: constraints [
 
	self widgetDo: [ :aPanel |
		self 
			basicAdd: aPresenter 
			constraints: constraints 
			to: aPanel ]
]

{ #category : #adding }
MorphicLayoutAdapter >> addConstraits: constraints toChild: childMorph [

	self subclassResponsibility
]

{ #category : #initialization }
MorphicLayoutAdapter >> applyStyle: aMorph [
	"layouts do not have style to apply"
]

{ #category : #factory }
MorphicLayoutAdapter >> applyStyle: aPresenter to: aMorph [
	self flag: #TODO. "This is a HACK, to apply styles just when I'm using new layouts 
	(otherwise, it interferes on how the old layout is displayed). 
	 Of course, this needs to be applied where it belongs, but for now we need to keep 
	 it this way :("
	
	aPresenter adapter ifNil: [ ^ self ].
	aPresenter adapter applyStyle: aMorph
]

{ #category : #private }
MorphicLayoutAdapter >> basicAdd: aPresenter constraints: constraints to: aPanel [ 

	self subclassResponsibility
]

{ #category : #private }
MorphicLayoutAdapter >> buildMorph: aPresenter constraints: constraints [ 
	| childMorph |

	childMorph := aPresenter buildWithSpec: constraints spec.
	self applyStyle: aPresenter to: childMorph.
	childMorph := self addConstraits: constraints toChild: childMorph.
	
	^ childMorph
]

{ #category : #factory }
MorphicLayoutAdapter >> buildWidget [

	^ nil
]

{ #category : #factory }
MorphicLayoutAdapter >> buildWithSpec [
	
	^ self widget
]

{ #category : #factory }
MorphicLayoutAdapter >> buildWithSpec: aString [ 

	^ self buildWithSpec
]
