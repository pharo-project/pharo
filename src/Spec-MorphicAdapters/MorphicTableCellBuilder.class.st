"
I build cells for a MorphicTableAdapter.
"
Class {
	#name : #MorphicTableCellBuilder,
	#superclass : #MorphicTableColumnVisitor,
	#instVars : [
		'dataSource',
		'rowIndex',
		'cell'
	],
	#category : #'Spec-MorphicAdapters-Table'
}

{ #category : #'instance creation' }
MorphicTableCellBuilder class >> on: aDataSource [

	^ self new dataSource: aDataSource
]

{ #category : #private }
MorphicTableCellBuilder >> addCell: anObject column: aTableColumn [
	| morph |

	anObject ifNil: [ ^ self ]. 
	
	morph := 	anObject asMorph.
	aTableColumn isExpandable ifFalse: [ 
		morph 
			hResizing: #rigid;
			width: (aTableColumn width ifNil: [ morph width ]) ].
	cell addMorphBack: morph.
]

{ #category : #private }
MorphicTableCellBuilder >> addCellColumn: aTableColumn [
	
	self 
		addCell: (aTableColumn readObject: self item) 
		column: aTableColumn
]

{ #category : #accessing }
MorphicTableCellBuilder >> cell [
	^ cell
]

{ #category : #accessing }
MorphicTableCellBuilder >> cell: anObject [
	cell := anObject
]

{ #category : #accessing }
MorphicTableCellBuilder >> dataSource [
	^ dataSource
]

{ #category : #accessing }
MorphicTableCellBuilder >> dataSource: anObject [
	dataSource := anObject
]

{ #category : #private }
MorphicTableCellBuilder >> item [ 

	^ self dataSource elementAt: self rowIndex
]

{ #category : #accessing }
MorphicTableCellBuilder >> rowIndex [
	^ rowIndex
]

{ #category : #accessing }
MorphicTableCellBuilder >> rowIndex: anObject [
	rowIndex := anObject
]

{ #category : #visiting }
MorphicTableCellBuilder >> visitCheckboxColumn: aTableColumn [
	self addCellColumn: aTableColumn
]

{ #category : #visiting }
MorphicTableCellBuilder >> visitImageColumn: aTableColumn [
	
	self addCellColumn: aTableColumn
]

{ #category : #visiting }
MorphicTableCellBuilder >> visitIndexColumn: aTableColumn [

	self 
		addCell: self rowIndex 
		column: aTableColumn
]

{ #category : #visiting }
MorphicTableCellBuilder >> visitStringColumn: aTableColumn [
	
	self addCellColumn: aTableColumn
]
