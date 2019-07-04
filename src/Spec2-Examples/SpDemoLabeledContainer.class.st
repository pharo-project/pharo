"
A component that should encapsulate a form part with a specific label
"
Class {
	#name : #SpDemoLabeledContainer,
	#superclass : #SpLabelledContainer,
	#category : #'Spec2-Examples-Demo-Support'
}

{ #category : #specs }
SpDemoLabeledContainer class >> defaultSpec [

	^ SpLayout composed
]

{ #category : #specs }
SpDemoLabeledContainer class >> labelTop [

	<spec: #default>

	| rowHeight |
	rowHeight := self inputTextHeight.

	^ SpLayout composed
		newColumn: [ :column | 
			column
				add: #label origin: 0@0 corner: 1@0 offsetOrigin: 0@0 offsetCorner: 0@rowHeight;
				add: #subwidget origin: 0@0 corner: 1@1];
		yourself
]

{ #category : #specs }
SpDemoLabeledContainer class >> labelTop2 [

	"<spec: #default>"

	^ SpLayout composed
		newColumn: [ :column | 
			column
				add: #label origin: 0 @ 0 corner: 1 @ 0.1;
				add: #subwidget origin: 0 @ 0.1 corner: 1 @ 1 ];
		yourself
]

{ #category : #specs }
SpDemoLabeledContainer class >> labelTop3 [

	"<spec: #default>"

	| rowHeight |
	rowHeight := self inputTextHeight.

	^ SpLayout composed
		newColumn: [ :column | 
			column newRow: [ :row |
				row add: #label 
				 ] height: rowHeight.
			column newRow: [ :row |
				row add: #subwidget 
				 ] height: rowHeight.
			];
		yourself
]

{ #category : #accessing }
SpDemoLabeledContainer >> label: aString [

	label label: ' - ', aString, ' - '
]
