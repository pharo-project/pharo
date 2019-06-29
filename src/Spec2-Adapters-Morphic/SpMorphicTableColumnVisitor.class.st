"
I'm a visitor to be used to traverse TableColumn hierarchy.
"
Class {
	#name : #SpMorphicTableColumnVisitor,
	#superclass : #Object,
	#category : #'Spec2-Adapters-Morphic-Table'
}

{ #category : #visiting }
SpMorphicTableColumnVisitor >> visit: aTableColumn [

	aTableColumn acceptColumnVisitor: self
]

{ #category : #visiting }
SpMorphicTableColumnVisitor >> visitCheckboxColumn: aCheckboxTableColumn [
]

{ #category : #visiting }
SpMorphicTableColumnVisitor >> visitCompositeColumn: aTableColumn [
	
	aTableColumn columns do: [ :each |
		self visit: each  ]
]

{ #category : #visiting }
SpMorphicTableColumnVisitor >> visitImageColumn: aTableColumn [
]

{ #category : #visiting }
SpMorphicTableColumnVisitor >> visitIndexColumn: aTableColumn [
]

{ #category : #visiting }
SpMorphicTableColumnVisitor >> visitStringColumn: aTableColumn [
]
