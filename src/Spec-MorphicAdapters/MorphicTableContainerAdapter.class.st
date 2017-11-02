"
I am the adapter providing container with table layout

"
Class {
	#name : #MorphicTableContainerAdapter,
	#superclass : #AbstractMorphicAdapter,
	#category : 'Spec-MorphicAdapters'
}

{ #category : #factory }
MorphicTableContainerAdapter >> buildWidget [
	^ PanelMorph new
		layoutPolicy: TableLayout new;
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		yourself
]

{ #category : #'spec protocol' }
MorphicTableContainerAdapter >> centerWidget: aWindow [
	self widgetDo: [ :w | w centerWidget: aWindow ]
]

{ #category : #protocol }
MorphicTableContainerAdapter >> height: aNumber [
	self vRigid.
	self widgetDo: [ :w | w height: aNumber ].
]

{ #category : #protocol }
MorphicTableContainerAdapter >> isRedrawable [
	"This must be overriden in the adapter representing your container"
	
	^ true
]

{ #category : #protocol }
MorphicTableContainerAdapter >> layoutProperties: props [
	self widgetDo: [ :w | w layoutProperties: props ]
]

{ #category : #protocol }
MorphicTableContainerAdapter >> minHeight: aNumber [
	self widgetDo: [ :w | w minHeight: aNumber ]
]

{ #category : #protocol }
MorphicTableContainerAdapter >> minWidth: aNumber [
	self widgetDo: [ :w | w minWidth: aNumber ]
]

{ #category : #protocol }
MorphicTableContainerAdapter >> width: aNumber [
	self hRigid.
	self widgetDo: [ :w | w width: aNumber ].
]
