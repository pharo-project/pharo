"
A SpecTableLayoutAddSpring class is a description of the addition of a spacer to a table layout.

Can be either:
- flexible (fills available space; default) 
- of a fixed size

Note that flexible spacers do not work with wrapDirection other than #none.

"
Class {
	#name : #SpecTableLayoutAddSpacer,
	#superclass : #SpecTableLayoutAdd,
	#instVars : [
		'commands',
		'spaceFillWeight',
		'size',
		'orientation'
	],
	#category : 'Spec-Layout'
}

{ #category : #'instance creation' }
SpecTableLayoutAddSpacer class >> sized: aNumber [
	^ self new size: aNumber
]

{ #category : #'instance creation' }
SpecTableLayoutAddSpacer class >> weighted: aNumber [
	^ self new spaceFillWeight: aNumber
]

{ #category : #protocol }
SpecTableLayoutAddSpacer >> asSpecElements [
	self generateArguments.
	^ super asSpecElements
]

{ #category : #'instance creation' }
SpecTableLayoutAddSpacer >> generateArguments [
	| resizingMode resizing width height dimensions |
	width := 0.
	height := 0.
	dimensions := OrderedCollection new.
	
	(size > 0) 
		ifTrue: [ "fixed size"
			(orientation == #horizontal) 
				ifTrue: [ width := size ] 
				ifFalse: [ height := size ].
			resizingMode := #rigid. ]
		ifFalse: [ "flexible"
			dimensions add: #spaceFillWeight:; add: spaceFillWeight.
			resizingMode := #spaceFill. ].
	
	dimensions add: #width:; add: width; add: #height:; add: height.
	
	resizing := 
		(orientation == #horizontal) 
			ifTrue: [ {#hResizing: . resizingMode . #vResizing: . #shrinkWrap} ]
			ifFalse: [ {#hResizing: . #shrinkWrap . #vResizing: . resizingMode} ].
	
	self arguments: { {subwidget} , resizing , dimensions }
]

{ #category : #initialization }
SpecTableLayoutAddSpacer >> initialize [
	subwidget := #(PanelMorph).
	spaceFillWeight := 1.
	size := 0.
	orientation := #vertical.
	super initialize
]

{ #category : #accessing }
SpecTableLayoutAddSpacer >> orientation: aSymbol [
	((aSymbol == #horizontal) | (aSymbol == #vertical))
		ifFalse: [ Error signal: 'orientation must be either #horizontal or #vertical' ]
		ifTrue: [ orientation := aSymbol ]
]

{ #category : #accessing }
SpecTableLayoutAddSpacer >> size: aNumber [
	size := aNumber
]

{ #category : #accessing }
SpecTableLayoutAddSpacer >> spaceFillWeight: aNumber [
	spaceFillWeight := aNumber
]
