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

	| labelMorphExtension cmt |
	labelMorphExtension := anItemCellMorph label assureExtension.
	"This datasourceitem actual object may be either a class or a ring class
	and guess what? their comments are not compatible!
	"
	
	cmt := aDataSourceItem actualObject isClassOrTrait
		ifTrue: [ aDataSourceItem actualObject comment]
		ifFalse: [ aDataSourceItem actualObject comment content ].
	cmt 
		ifEmpty: [ cmt := ClyUncommentedItemTableDecorator balloonText ]
		ifNotEmpty: [ | lines |
				lines := cmt lines. 
				cmt := lines first: (lines size min: 3).
				cmt := String cr join: cmt. ].
	labelMorphExtension balloonText: cmt 
		
		
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
