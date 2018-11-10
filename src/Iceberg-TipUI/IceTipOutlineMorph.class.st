"
I'm a morph to display outlines (with outline datasources).

- roots are not collapsable (and then, they are expanded by default).
- I prepare a certain look and feel (they are transparent).

"
Class {
	#name : #IceTipOutlineMorph,
	#superclass : #FTTableMorph,
	#category : #'Iceberg-TipUI-View-DataSource'
}

{ #category : #accessing }
IceTipOutlineMorph >> dataSource: aDataSource [
	super dataSource: aDataSource.
	aDataSource expandRoots.
]

{ #category : #drawing }
IceTipOutlineMorph >> drawKeyboardFocusOn: canvas [
	"no focus mark"
]

{ #category : #initialization }
IceTipOutlineMorph >> initialize [
	super initialize.
	self color: Color transparent
]
