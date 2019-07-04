"
I decorate deprecated methods in the table with struck out font
"
Class {
	#name : #ClyDeprecatedItemTableDecorator,
	#superclass : #ClyTableDecorator,
	#category : #'Calypso-SystemPlugins-Deprecation-Browser'
}

{ #category : #decoration }
ClyDeprecatedItemTableDecorator class >> browserDecorationStrategy [
	<classAnnotation>
	
	^ClyTableDecorationStrategy for: ClySystemBrowserContext
]

{ #category : #decoration }
ClyDeprecatedItemTableDecorator class >> decorateTableCell: anItemCellMorph of: aDataSourceItem [
	| nameMorph |

	nameMorph := anItemCellMorph label.
	nameMorph clyAddEmphasis: TextEmphasis struckOut
]

{ #category : #decoration }
ClyDeprecatedItemTableDecorator class >> wantsDecorateTableCellOf: aDataSourceItem [
	^aDataSourceItem isMarkedWith: ClyDeprecatedItemTag
]
