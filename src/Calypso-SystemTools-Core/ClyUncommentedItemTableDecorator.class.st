"
I decorate table with special icon for classes which has no comment.

I add this icon as an extra tool of table cell. So it do not override and do not overridden by other decorators.
"
Class {
	#name : #ClyUncommentedItemTableDecorator,
	#superclass : #ClyTableDecorator,
	#category : #'Calypso-SystemTools-Core-TableDecorators'
}

{ #category : #decoration }
ClyUncommentedItemTableDecorator class >> decorateMainTableCell: anItemCellMorph of: aDataSourceItem [
	| noCommentWarn |
	super decorateMainTableCell: anItemCellMorph of: aDataSourceItem.
	
	noCommentWarn := anItemCellMorph addExtraIcon: #uncommentedClass.
	noCommentWarn setBalloonText: 'There is no comment!'
]

{ #category : #decoration }
ClyUncommentedItemTableDecorator class >> decorateTableCell: anItemCellMorph of: aDataSourceItem [
	
]

{ #category : #decoration }
ClyUncommentedItemTableDecorator class >> decorationStrategy [
	<classAnnotation>
	
	^ClyTableDecorationStrategy for: ClyClass asCalypsoItemContext
]

{ #category : #decoration }
ClyUncommentedItemTableDecorator class >> wantsDecorateTableCellOf: aDataSourceItem [
	^aDataSourceItem isMarkedWith: ClyNoCommentTag
]
