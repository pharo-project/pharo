"
A component that should encapsulate a form part with a specific label
"
Class {
	#name : #SpecDemoLabeledContainer,
	#superclass : #LabelledContainer,
	#category : #'Spec-Examples-Demo-Support'
}

{ #category : #specs }
SpecDemoLabeledContainer class >> defaultSpec [

	^ SpecLayout composed
]

{ #category : #specs }
SpecDemoLabeledContainer class >> labelTop [

	<spec: #default>

	| rowHeight |
	rowHeight := self inputTextHeight.

	^ SpecLayout composed
		newColumn: [ :column | 
			column
				add: #label origin: 0@0 corner: 1@0 offsetOrigin: 0@0 offsetCorner: 0@rowHeight;
				add: #subwidget origin: 0@0 corner: 1@1];
		yourself
]

{ #category : #specs }
SpecDemoLabeledContainer class >> labelTop2 [

	"<spec: #default>"

	^ SpecLayout composed
		newColumn: [ :column | 
			column
				add: #label origin: 0 @ 0 corner: 1 @ 0.1;
				add: #subwidget origin: 0 @ 0.1 corner: 1 @ 1 ];
		yourself
]

{ #category : #specs }
SpecDemoLabeledContainer class >> labelTop3 [

	"<spec: #default>"

	| rowHeight |
	rowHeight := self inputTextHeight.

	^ SpecLayout composed
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
SpecDemoLabeledContainer >> label: aString [

	label label: ' - ', aString, ' - '
]
