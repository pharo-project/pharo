querying
entryByPath: aString
	^ (aString = '.' )
		ifTrue: [ self ]
		ifFalse: [( ($/ split: aString) reject: #isEmpty)
			inject: self 
			into: [ :treeBuilder :name | treeBuilder entryByName: name ]]