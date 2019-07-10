"
I am the adapter providing container with table layout

"
Class {
	#name : #SpMorphicTableContainerAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #factory }
SpMorphicTableContainerAdapter >> buildWidget [
	^ PanelMorph new
		layoutPolicy: TableLayout new;
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		yourself
]

{ #category : #protocol }
SpMorphicTableContainerAdapter >> height: aNumber [
	self vRigid.
	self widgetDo: [ :w | w height: aNumber ].
]

{ #category : #protocol }
SpMorphicTableContainerAdapter >> isRedrawable [
	"This must be overriden in the adapter representing your container"
	
	^ true
]

{ #category : #protocol }
SpMorphicTableContainerAdapter >> layoutProperties: props [
	self widgetDo: [ :w | w layoutProperties: props ]
]

{ #category : #protocol }
SpMorphicTableContainerAdapter >> minHeight: aNumber [
	self widgetDo: [ :w | w minHeight: aNumber ]
]

{ #category : #protocol }
SpMorphicTableContainerAdapter >> minWidth: aNumber [
	self widgetDo: [ :w | w minWidth: aNumber ]
]

{ #category : #protocol }
SpMorphicTableContainerAdapter >> width: aNumber [
	self hRigid.
	self widgetDo: [ :w | w width: aNumber ].
]
