"
I'm a column for Tip tables. 
I can read a value on an object using an action block or symbol.
"
Class {
	#name : #IceTipTableColumn,
	#superclass : #FTColumn,
	#instVars : [
		'action',
		'icon'
	],
	#category : #'Iceberg-TipUI-View-DataSource'
}

{ #category : #'instance creation' }
IceTipTableColumn class >> id: aString action: aBlock width: aNumber [
	^ self new
		id: aString;
		action: aBlock;
		width: aNumber;
		yourself
]

{ #category : #'instance creation' }
IceTipTableColumn class >> newAction: aSymbol [
	^ self new action: aSymbol
]

{ #category : #'instance creation' }
IceTipTableColumn class >> newAction: aSymbolOrBlock icon: iconSymbolOrBlock [
	^ self new 
		action: aSymbolOrBlock;
		icon: iconSymbolOrBlock;
		yourself
]

{ #category : #accessing }
IceTipTableColumn >> action [
	^ action
]

{ #category : #accessing }
IceTipTableColumn >> action: aSymbolOrBlock [
	action := aSymbolOrBlock
]

{ #category : #accessing }
IceTipTableColumn >> icon [
	^ icon
]

{ #category : #accessing }
IceTipTableColumn >> icon: aSymbolOrBlock [
	icon := aSymbolOrBlock
]

{ #category : #factory }
IceTipTableColumn >> newHeader [
	^ SimpleButtonMorph new
		layoutPolicy: RowLayout new; 
		listDirection: #leftToRight; 
		wrapCentering: #center;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		cellInset: 5@0;
		layoutInset: 5@0;
		label: self id;
		yourself
]

{ #category : #accessing }
IceTipTableColumn >> read: anObject [
	^ self action value: anObject
]

{ #category : #accessing }
IceTipTableColumn >> readIcon: anObject [
	^ self icon 
		ifNotNil: [ self icon value: anObject ]
]
