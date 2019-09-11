"
I decorate classes with their icons defined by #systemIcon method.

My decoration logic is a bit complex. I am supposed to work in remote browser too. And sending #systemIcon to remote class would be very expensive.
So instead I find same class in my local environment and ask it for the icon.
"
Class {
	#name : #ClyClassIconTableDecorator,
	#superclass : #ClyClassTableDecorator,
	#category : #'Calypso-SystemTools-Core-TableDecorators'
}

{ #category : #decoration }
ClyClassIconTableDecorator class >> decorateMainTableCell: anItemCellMorph of: aDataSourceItem [
	
	| icon |
	super decorateMainTableCell: anItemCellMorph of: aDataSourceItem.
	    
   icon := Object environment at: aDataSourceItem name asSymbol
            ifPresent: [ :aClass | aClass systemIcon ]
            ifAbsent: [ Object systemIcon ].

    anItemCellMorph definitionMorph: icon asMorph
]

{ #category : #decoration }
ClyClassIconTableDecorator class >> decorateTableCell: anItemCellMorph of: aDataSourceItem [
]
