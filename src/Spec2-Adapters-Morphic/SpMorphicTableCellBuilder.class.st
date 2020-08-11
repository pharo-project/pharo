"
I build cells for a MorphicTableAdapter.
"
Class {
	#name : #SpMorphicTableCellBuilder,
	#superclass : #SpMorphicTableColumnVisitor,
	#instVars : [
		'dataSource',
		'rowIndex',
		'cell'
	],
	#category : #'Spec2-Adapters-Morphic-Table'
}

{ #category : #'instance creation' }
SpMorphicTableCellBuilder class >> on: aDataSource [

	^ self new dataSource: aDataSource
]

{ #category : #private }
SpMorphicTableCellBuilder >> addCell: anObject column: aTableColumn [
	| morph |

	anObject ifNil: [ ^ self ]. 
	
	morph := 	anObject asMorph asReadOnlyMorph.
	self 
		addCellMorph: morph 
		column: aTableColumn
]

{ #category : #private }
SpMorphicTableCellBuilder >> addCellColumn: aTableColumn [
	
	self 
		addCell: (aTableColumn readObject: self item) 
		column: aTableColumn
]

{ #category : #private }
SpMorphicTableCellBuilder >> addCellMorph: morph column: aTableColumn [

	aTableColumn isExpandable ifFalse: [ 
		aTableColumn width: (aTableColumn width 
			ifNotNil: [ :width | width max: morph width ]
			ifNil: [ morph width ]).
		cell hResizing: #shrinkWrap.
		morph 
			hResizing: #rigid;
			width: aTableColumn width ].
	
	cell addMorphBack: morph
	
]

{ #category : #accessing }
SpMorphicTableCellBuilder >> cell [
	^ cell
]

{ #category : #accessing }
SpMorphicTableCellBuilder >> cell: anObject [
	cell := anObject
]

{ #category : #accessing }
SpMorphicTableCellBuilder >> dataSource [
	^ dataSource
]

{ #category : #accessing }
SpMorphicTableCellBuilder >> dataSource: anObject [
	dataSource := anObject
]

{ #category : #private }
SpMorphicTableCellBuilder >> item [ 

	^ self dataSource elementAt: self rowIndex
]

{ #category : #accessing }
SpMorphicTableCellBuilder >> rowIndex [
	^ rowIndex
]

{ #category : #accessing }
SpMorphicTableCellBuilder >> rowIndex: anObject [
	rowIndex := anObject
]

{ #category : #visiting }
SpMorphicTableCellBuilder >> visitCheckboxColumn: aTableColumn [
	| presenter morph |
	
	presenter := SpCheckBoxPresenter new
		addStyle: 'compact';
		state: (aTableColumn readObject: self item);
		yourself.

	aTableColumn onActivation ifNotNil: [ :valuable | 
		presenter whenActivatedDo: [ valuable cull: self item ] ].
	aTableColumn onDeactivation ifNotNil: [ :valuable | 
		presenter whenDeactivatedDo: [ valuable cull: self item ] ].

	morph := presenter buildWithSpec.
	presenter adapter applyStyle: morph.

	self 
		addCellMorph: morph
		column: aTableColumn 
]

{ #category : #visiting }
SpMorphicTableCellBuilder >> visitImageColumn: aTableColumn [
	
	self addCellColumn: aTableColumn
]

{ #category : #visiting }
SpMorphicTableCellBuilder >> visitIndexColumn: aTableColumn [

	self 
		addCell: self rowIndex 
		column: aTableColumn
]

{ #category : #visiting }
SpMorphicTableCellBuilder >> visitStringColumn: aTableColumn [
	
	self addCellColumn: aTableColumn
]
