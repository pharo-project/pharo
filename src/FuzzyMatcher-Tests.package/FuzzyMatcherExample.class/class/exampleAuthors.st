as yet unclassified
exampleAuthors

	| authors blk |
	
	authors := OrderedCollection new.
	blk := [ :mtd | 
		mtd timeStamp ifNotEmpty: [ :stamp |
			(' ' join: ((stamp splitOn: ' ') allButLast: 2)) ifNotEmpty: [ :author |
				 author trimmed ifNotEmpty: [ authors add: author ]
			]
		] 
	].
	
	Smalltalk allClassesAndTraitsDo: [ :each |
		each methodsDo: blk. 
		each theMetaClass methodsDo: blk.
	].

	^ self new 
		elements: authors asSet asArray;
		open