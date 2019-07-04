"
I am the adapter providing the correct container class: PanelMorph
"
Class {
	#name : #SpMorphicContainerAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #factory }
SpMorphicContainerAdapter >> buildWidget [

	^ PanelMorph new
		changeProportionalLayout;
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		yourself
]

{ #category : #protocol }
SpMorphicContainerAdapter >> checkSplitters [
	
	self widgetDo: [ :w | w checkSplitters ]
]

{ #category : #protocol }
SpMorphicContainerAdapter >> isRedrawable [
	"This must be overriden in the adapter representing your container"
	
	^ true
]

{ #category : #protocol }
SpMorphicContainerAdapter >> newHSplitterAt: aPosition [
	
	self widgetDo: [ :w | w newHSplitterAt: aPosition ]
]

{ #category : #protocol }
SpMorphicContainerAdapter >> newVSplitterAt: aPosition [
	
	self widgetDo: [ :w | w newVSplitterAt: aPosition ]
]
