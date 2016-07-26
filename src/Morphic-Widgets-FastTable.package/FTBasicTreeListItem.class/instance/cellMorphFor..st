accessing
cellMorphFor: aDataSource

	| morph |
	
	morph := FTIndentedCellMorph new.
	morph depthIndent: depth.
	
	self hasChildren 
		ifTrue: [ morph addMorphBack: self generateButton ]
		ifFalse: [ morph addIndendtation: 15 ].
	
	dataSource displayBlock value: item value: morph.
	
	^morph