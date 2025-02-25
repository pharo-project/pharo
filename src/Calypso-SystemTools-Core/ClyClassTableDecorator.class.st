"
I am a base class for class table decorators.

I am annotated by decoration strategy in context of selected class.
So my subclasses should just implement decoration logic
"
Class {
	#name : 'ClyClassTableDecorator',
	#superclass : 'ClyTableDecorator',
	#category : 'Calypso-SystemTools-Core-TableDecorators',
	#package : 'Calypso-SystemTools-Core',
	#tag : 'TableDecorators'
}

{ #category : 'decoration' }
ClyClassTableDecorator class >> decorateTableCell: anItemCellMorph of: aDataSourceItem [

	| labelMorphExtension |
	labelMorphExtension := anItemCellMorph label assureExtension.
	labelMorphExtension balloonText:
		(aDataSourceItem actualObject comment ifEmpty: [
			 ClyUncommentedItemTableDecorator balloonText ])
]

{ #category : 'decoration' }
ClyClassTableDecorator class >> decorationStrategy [
	<classAnnotation>

	^ClyTableDecorationStrategyAnnotation for: ClyClass asCalypsoItemContext
]

{ #category : 'testing' }
ClyClassTableDecorator class >> isAbstract [
	^self = ClyClassTableDecorator
]
