"
I decorate table items with red font for methods which use undeclared variables
"
Class {
	#name : #ClyUndeclaredMethodTableDecorator,
	#superclass : #ClyMethodTableDecorator,
	#category : #'Calypso-SystemPlugins-Undeclared-Browser'
}

{ #category : #decoration }
ClyUndeclaredMethodTableDecorator class >> decorateTableCell: anItemCellMorph of: aDataSourceItem [
	| nameMorph |

	nameMorph := anItemCellMorph label.
	nameMorph color: Color red
]

{ #category : #testing }
ClyUndeclaredMethodTableDecorator class >> wantsDecorateTableCellOf: aDataSourceItem [
	^aDataSourceItem isMarkedWith: ClyUndeclaresUserTag
]
