"
I build cells for a MorphicTreeTableAdapter.
"
Class {
	#name : #MorphicTreeTableCellBuilder,
	#superclass : #MorphicTableCellBuilder,
	#instVars : [
		'item'
	],
	#category : #'Spec-MorphicAdapters-Table'
}

{ #category : #accessing }
MorphicTreeTableCellBuilder >> item [
	
	^ item
]

{ #category : #accessing }
MorphicTreeTableCellBuilder >> item: anObject [

	item := anObject
]
