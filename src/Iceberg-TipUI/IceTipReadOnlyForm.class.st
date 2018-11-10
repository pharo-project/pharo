"
I'm a ""read only form"", used to display data in a more readable way than a list morph or a panel morph.
I receive a list of ""label"" and ""values"", being label a string and value a string or text object.

I can show lists of elements as this example shows: 

IceTipReadOnlyForm new 
	addLabel: 'Commit:' value: 'a test';
	addLabel: 'Parents:' value: '123456789';
	addLabel: 'Author:' value: 'Some Author <author@email.net>';
	addLabel: 'Date:' value: DateAndTime now asStringYMDHM;
	addLabel: 'Labels:' value: '';
	addLabel: 'Comment' value: String loremIpsum;
	openWithSpec.
	
"
Class {
	#name : #IceTipReadOnlyForm,
	#superclass : #DynamicComposablePresenter,
	#instVars : [
		'items',
		'labelColor',
		'labelWidth',
		'rowHeight'
	],
	#category : #'Iceberg-TipUI-Spec-ReadOnlyForm'
}

{ #category : #accessing }
IceTipReadOnlyForm class >> defaultLabelWidth [
	^ 100 * World displayScaleFactor
]

{ #category : #accessing }
IceTipReadOnlyForm class >> defaultRowHeight [
	^ 25 * World displayScaleFactor
]

{ #category : #specs }
IceTipReadOnlyForm class >> defaultSpec [
	<spec: #default>
	
	^ SpecLayout composed
]

{ #category : #'instance creation' }
IceTipReadOnlyForm class >> items: aDictionary [
	"If you want to keep order, just send an OrderedDictionary"
	^ self new items: aDictionary
]

{ #category : #accessing }
IceTipReadOnlyForm >> addLabel: aString value: anObject [
	items at: aString put: anObject.
	self refresh
]

{ #category : #private }
IceTipReadOnlyForm >> addRow: labelId value: valueId isLast: isLast to: column [
	isLast
		ifTrue: [
			column 
				newRow: [ :row | 
					row 
						newColumn: labelId width: self iceLabelWidth; 
						newColumn: valueId ] ]
		ifFalse: [  
			column 
				newRow: [ :row | 
						row 
							newColumn: labelId width: self iceLabelWidth; 
							newColumn: valueId ]
				height: self rowHeight ]
]

{ #category : #private }
IceTipReadOnlyForm >> addWidgetsTo: column [
	| last |
	last := items associations last.
	items associationsDo: [ :each | | label value |
		label := each key.
		value := each value.
		self 
			newWidgetLabel: label 
			value: value 
			do: [ :labelId :valueId |
				self 
					addRow: labelId 
					value: valueId 
					isLast: last = each
					to: column ] ]
]

{ #category : #accessing }
IceTipReadOnlyForm >> iceLabelWidth [
	^ labelWidth ifNil: [ self class defaultLabelWidth ]
]

{ #category : #initialization }
IceTipReadOnlyForm >> initialize [
	items := OrderedDictionary new.
	super initialize

]

{ #category : #initialization }
IceTipReadOnlyForm >> initializeWidgets [
]

{ #category : #accessing }
IceTipReadOnlyForm >> items: aDictionary [
	items removeAll.
	items addAll: aDictionary.
	self refresh
]

{ #category : #accessing }
IceTipReadOnlyForm >> labelColor [
	^ labelColor ifNil: [ labelColor := Smalltalk ui theme textColor ]
]

{ #category : #accessing }
IceTipReadOnlyForm >> labelColor: aColor [
	^ labelColor := aColor
]

{ #category : #accessing }
IceTipReadOnlyForm >> labelWidth: aNumber [
	^ labelWidth := aNumber
]

{ #category : #'private factory' }
IceTipReadOnlyForm >> newLabel: aString [
	^ self newPanelMorph
		listDirection: #rightToLeft;
		cellPositioning: #center;
		addMorph: ((LabelMorph newLabel: aString)
			color: self labelColor;
			yourself);
		asSpecAdapter
]

{ #category : #'private factory' }
IceTipReadOnlyForm >> newLayout [
	^ SpecLayout composed 
		newColumn: [ :column | ]
]

{ #category : #'private factory' }
IceTipReadOnlyForm >> newPanelMorph [
	^ PanelMorph new 
		changeTableLayout;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		yourself
]

{ #category : #'private factory' }
IceTipReadOnlyForm >> newValue: aStringOrText [
	^ IceTipReadOnlyTextMorph new 
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		contentsWrapped: aStringOrText;
		asSpecAdapter
]

{ #category : #'private factory' }
IceTipReadOnlyForm >> newWidgetLabel: labelString value: textOrString do: aBlock [
	| index labelId valueId |
	
	index := items indexOfKey: labelString.

	labelId := ('label_', index asString) asSymbol.
	valueId := ('value_', index asString) asSymbol.
	widgets at: labelId put: (self newLabel: labelString).
	widgets at: valueId put: (self newValue: textOrString).	
	aBlock value: labelId value: valueId
]

{ #category : #private }
IceTipReadOnlyForm >> refresh [
	widgets removeAll.
	self layout: self newLayout.
	self layout	
		newColumn: [ :column | self addWidgetsTo: column ]
]

{ #category : #accessing }
IceTipReadOnlyForm >> rowHeight [
	^ rowHeight ifNil: [ rowHeight := self class defaultRowHeight ]
]

{ #category : #accessing }
IceTipReadOnlyForm >> rowHeight: aNumber [
	rowHeight := aNumber
]
