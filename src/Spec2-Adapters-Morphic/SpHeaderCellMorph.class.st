"
I am a header cell.
I am used to allow a header to have some extra properties (sort, etc.).

Instead using a FTHeaderColumnCellMorph I extent FTIndentedCellMorph becaue I need the text to be indented.
"
Class {
	#name : #SpHeaderCellMorph,
	#superclass : #FTIndentedCellMorph,
	#category : #'Spec2-Adapters-Morphic-Table'
}

{ #category : #accessing }
SpHeaderCellMorph class >> indent [

	^ 2
]

{ #category : #initialization }
SpHeaderCellMorph >> initialize [

	super initialize.
	self listCentering: #left.
	self indentBy: self class indent
]

{ #category : #'as yet unclassified' }
SpHeaderCellMorph >> prepareAsSortableColumn: aColumn [

	
]
