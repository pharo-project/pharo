"
Better use 

| iconList |
	iconList := SpListPresenter new.
	iconList
		items: Smalltalk ui icons icons associations;
		displayBlock: [ :assoc | assoc key ];
		sortingBlock: [ :assocA :assocB | assocA key < assocB key ];
		icons: [ :elem | elem value ].
 iconList openWithSpec


"
Class {
	#name : #SpIconListPresenter,
	#superclass : #SpListPresenter,
	#category : #'Spec2-Deprecated-Replaced'
}

{ #category : #example }
SpIconListPresenter class >> example [
	<sampleInstance>
	| example |
	example := self new.
	example
		icons: [ :e | example iconNamed: #forwardIcon ];
		items: (1 to: 10) asArray;
		openWithSpec.
	^ example
]

{ #category : #deprecation }
SpIconListPresenter class >> isDeprecated [
	^ true
]
