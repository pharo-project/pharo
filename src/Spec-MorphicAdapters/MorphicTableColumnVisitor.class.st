"
I'm a visitor to be used to traverse TableColumn hierarchy.
"
Class {
	#name : #MorphicTableColumnVisitor,
	#superclass : #Object,
	#category : #'Spec-MorphicAdapters-Table'
}

{ #category : #visiting }
MorphicTableColumnVisitor >> visit: aTableColumn [

	aTableColumn acceptColumnVisitor: self
]

{ #category : #visiting }
MorphicTableColumnVisitor >> visitCompositeColumn: aTableColumn [
	
	aTableColumn columns do: [ :each |
		self visit: each  ]
]

{ #category : #visiting }
MorphicTableColumnVisitor >> visitImageColumn: aTableColumn [
]

{ #category : #visiting }
MorphicTableColumnVisitor >> visitIndexColumn: aTableColumn [
]

{ #category : #visiting }
MorphicTableColumnVisitor >> visitStringColumn: aTableColumn [
]
