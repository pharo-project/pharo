"
I decorate table with simple constant icon for class comments.
For example query browser can show ClyClassComment instances in the table
"
Class {
	#name : #ClyClassCommentTableDecorator,
	#superclass : #ClyTableDecorator,
	#category : #'Calypso-SystemTools-Core-TableDecorators'
}

{ #category : #decoration }
ClyClassCommentTableDecorator class >> decorateMainTableCell: anItemCellMorph of: aDataSourceItem [
	super decorateMainTableCell: anItemCellMorph of: aDataSourceItem.

	anItemCellMorph definitionIcon: #commentsIcon
]

{ #category : #decoration }
ClyClassCommentTableDecorator class >> decorateTableCell: anItemCellMorph of: aDataSourceItem [

]

{ #category : #decoration }
ClyClassCommentTableDecorator class >> decorationStrategy [
	<classAnnotation>
	
	^ClyTableDecorationStrategy for: ClyClassComment asCalypsoItemContext
]
