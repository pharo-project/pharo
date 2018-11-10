"
I'm a filter function that will expand all children of a tree when updating. 
Of course, I'm to be used when searching tree structures.
"
Class {
	#name : #IceTipTreeFilterFunction,
	#superclass : #FTFilterFunction,
	#category : #'Iceberg-TipUI-View-DataSource'
}

{ #category : #updating }
IceTipTreeFilterFunction >> filter [
	pattern ifNil: [ ^ self ].

	table dataSource: (pattern 
		ifNotEmpty: [ initialDataSource newDataSourceMatching: (filterClass pattern: pattern) ]
		ifEmpty: [ initialDataSource ]).
	table selectRowIndexes: #().
	table dataSource expandAll.

	self isExplicite
		ifTrue: [ self resizeWidget ]
]
