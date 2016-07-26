private
packagesAsNodes
	^ (self model packages
		select: [ :each | (self packageMatchPattern: each) and: [ ((each name beginsWith: 'ConfigurationOf') or: [ each name beginsWith: 'BaselineOf' ]) not ] ]
		thenCollect: [ :each | each asNautilusNodeWithModel: self ]) sorted: [ :a :b | a name < b name ]