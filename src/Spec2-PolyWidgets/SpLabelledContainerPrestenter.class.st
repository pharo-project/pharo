"
I am an elementary Spec widget that adds a LabelPresenter at another widget.

self example

I provide the method content: that take a class in parameter, it instantiates it in the variable subwidget and add it at the focusOrder. Note it does not reset the focusOrder.

I provide accessors for the label and the subwidget, but I specialize doesNotUnderstand: to send the message at the subwidget.

I provide four layouts on my class side, to chose the position of the label.
"
Class {
	#name : #SpLabelledContainerPrestenter,
	#superclass : #SpPresenter,
	#instVars : [
		'label',
		'subwidget'
	],
	#category : #'Spec2-PolyWidgets-Labelled'
}

{ #category : #specs }
SpLabelledContainerPrestenter class >> defaultSpec [
	^ self labelTop
]

{ #category : #specs }
SpLabelledContainerPrestenter class >> labelBottom [
	<spec>
	^ SpLayout composed
		newColumn: [ :column | 
			column
				add: #subwidget origin: 0 @ 0 corner: 1 @ 0.9;
				add: #label origin: 0 @ 0.9 corner: 1 @ 1 ];
		yourself
]

{ #category : #specs }
SpLabelledContainerPrestenter class >> labelLeft [
	<spec>
	^ SpLayout composed
		newRow: [ :row | 
			row
				add: #label origin: 0 @ 0 corner: 0.2 @ 1;
				add: #subwidget origin: 0.2 @ 0 corner: 1 @ 1 ];
		yourself
]

{ #category : #specs }
SpLabelledContainerPrestenter class >> labelRight [
	<spec>
	^ SpLayout composed
		newRow: [ :row | 
			row
				add: #subwidget origin: 0 @ 0 corner: 0.8 @ 1;
				add: #label origin: 0.8 @ 0 corner: 1 @ 1 ];
		yourself
]

{ #category : #specs }
SpLabelledContainerPrestenter class >> labelTop [
	<spec>
	^ SpLayout composed
		newColumn: [ :column | 
			column
				add: #label origin: 0 @ 0 corner: 1 @ 0.1;
				add: #subwidget origin: 0 @ 0.1 corner: 1 @ 1 ];
		yourself
]

{ #category : #initialization }
SpLabelledContainerPrestenter >> content: aWidgetClass [
	subwidget := self instantiate: aWidgetClass.
	self focusOrder add: subwidget
]

{ #category : #'error handling' }
SpLabelledContainerPrestenter >> doesNotUnderstand: aMessage [
	^ aMessage sendTo: subwidget
]

{ #category : #initialization }
SpLabelledContainerPrestenter >> initializeWidgets [
	label := self instantiate: SpLabelPresenter.
]

{ #category : #accessing }
SpLabelledContainerPrestenter >> label [
	^ label
]

{ #category : #accessing }
SpLabelledContainerPrestenter >> label: aString [
	label label: aString
]

{ #category : #accessing }
SpLabelledContainerPrestenter >> subwidget [
	^ subwidget
]

{ #category : #accessing }
SpLabelledContainerPrestenter >> subwidget: anObject [
	subwidget := anObject
]
