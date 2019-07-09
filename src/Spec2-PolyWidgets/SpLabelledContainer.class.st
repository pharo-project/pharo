"
I am an elementary Spec widget that adds a LabelPresenter at another widget.

self example

I provide the method content: that take a class in parameter, it instantiates it in the variable subwidget and add it at the focusOrder. Note it does not reset the focusOrder.

I provide accessors for the label and the subwidget, but I specialize doesNotUnderstand: to send the message at the subwidget.

I provide four layouts on my class side, to chose the position of the label.
"
Class {
	#name : #SpLabelledContainer,
	#superclass : #SpPresenter,
	#instVars : [
		'label',
		'subwidget'
	],
	#category : #'Spec2-PolyWidgets-Labelled'
}

{ #category : #specs }
SpLabelledContainer class >> defaultSpec [
	^ self labelTop
]

{ #category : #specs }
SpLabelledContainer class >> labelBottom [
	<spec>
	^ SpLayout composed
		newColumn: [ :column | 
			column
				add: #subwidget origin: 0 @ 0 corner: 1 @ 0.9;
				add: #label origin: 0 @ 0.9 corner: 1 @ 1 ];
		yourself
]

{ #category : #specs }
SpLabelledContainer class >> labelLeft [
	<spec>
	^ SpLayout composed
		newRow: [ :row | 
			row
				add: #label origin: 0 @ 0 corner: 0.2 @ 1;
				add: #subwidget origin: 0.2 @ 0 corner: 1 @ 1 ];
		yourself
]

{ #category : #specs }
SpLabelledContainer class >> labelRight [
	<spec>
	^ SpLayout composed
		newRow: [ :row | 
			row
				add: #subwidget origin: 0 @ 0 corner: 0.8 @ 1;
				add: #label origin: 0.8 @ 0 corner: 1 @ 1 ];
		yourself
]

{ #category : #specs }
SpLabelledContainer class >> labelTop [
	<spec>
	^ SpLayout composed
		newColumn: [ :column | 
			column
				add: #label origin: 0 @ 0 corner: 1 @ 0.1;
				add: #subwidget origin: 0 @ 0.1 corner: 1 @ 1 ];
		yourself
]

{ #category : #initialization }
SpLabelledContainer >> content: aWidgetClass [
	subwidget := self instantiate: aWidgetClass.
	self focusOrder add: subwidget
]

{ #category : #'error handling' }
SpLabelledContainer >> doesNotUnderstand: aMessage [
	^ aMessage sendTo: subwidget
]

{ #category : #initialization }
SpLabelledContainer >> initializeWidgets [
	label := self instantiate: SpLabelPresenter.
]

{ #category : #accessing }
SpLabelledContainer >> label [
	^ label
]

{ #category : #accessing }
SpLabelledContainer >> label: aString [
	label label: aString
]

{ #category : #accessing }
SpLabelledContainer >> subwidget [
	^ subwidget
]

{ #category : #accessing }
SpLabelledContainer >> subwidget: anObject [
	subwidget := anObject
]
