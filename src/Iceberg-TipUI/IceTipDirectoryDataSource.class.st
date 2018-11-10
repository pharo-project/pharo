Class {
	#name : #IceTipDirectoryDataSource,
	#superclass : #IceTipTreeDataSource,
	#category : #'Iceberg-TipUI-View-Project'
}

{ #category : #accessing }
IceTipDirectoryDataSource >> childrenToShowFrom: aDirectory [
	
	^ (aDirectory children select: [ :each |
		each isDirectory and: [ (each basename beginsWith: '.') not ] ])
			sort: [ :a :b | a basename < b basename ]
]

{ #category : #accessing }
IceTipDirectoryDataSource >> directory: aDirectory [

	rootItem := (self class rootItemFor: { aDirectory })
		dataSource: self;
		yourself.
	self rootItem: rootItem
]

{ #category : #accessing }
IceTipDirectoryDataSource >> iconFor: aDirectory [

	^ self iconNamed: #emptyPackage
]

{ #category : #accessing }
IceTipDirectoryDataSource >> initialize [

	super initialize.
	self childrenBlock: [ :directory | 
		directory isDirectory
			ifTrue: [ self childrenToShowFrom: directory ]
			ifFalse: [ #() ]].
		
]

{ #category : #accessing }
IceTipDirectoryDataSource >> newCellFor: item [
	| cell |
	
	cell := FTIndentedCellMorph new.
	cell indentBy: item depth * 16.
	cell addMorphBack: (self buttonFor: item).
	(self iconFor: item data) 
		ifNotNil: [ :icon | cell addMorphBack: icon asMorph ].
	cell addMorphBack: (self toString: item data) asMorph.
	^ cell
]

{ #category : #accessing }
IceTipDirectoryDataSource >> toString: aFileReference [

	^ aFileReference basename
]
