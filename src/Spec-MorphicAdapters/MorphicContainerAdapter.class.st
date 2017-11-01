"
I am the adapter providing the correct container class: PanelMorph
"
Class {
	#name : #MorphicContainerAdapter,
	#superclass : #AbstractMorphicAdapter,
	#category : 'Spec-MorphicAdapters'
}

{ #category : #factory }
MorphicContainerAdapter >> buildWidget [

	^ PanelMorph new
		changeProportionalLayout;
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		yourself
]

{ #category : #'spec protocol' }
MorphicContainerAdapter >> centerWidget: aWindow [

	self widgetDo: [ :w| w centerWidget: aWindow ]
]

{ #category : #protocol }
MorphicContainerAdapter >> checkSplitters [
	
	self widgetDo: [ :w | w checkSplitters ]
]

{ #category : #protocol }
MorphicContainerAdapter >> isRedrawable [
	"This must be overriden in the adapter representing your container"
	
	^ true
]

{ #category : #protocol }
MorphicContainerAdapter >> newHSplitterAt: aPosition [
	
	self widgetDo: [ :w | w newHSplitterAt: aPosition ]
]

{ #category : #protocol }
MorphicContainerAdapter >> newVSplitterAt: aPosition [
	
	self widgetDo: [ :w | w newVSplitterAt: aPosition ]
]
