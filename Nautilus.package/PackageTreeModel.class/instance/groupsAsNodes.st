private
groupsAsNodes
	^ (self model groups collect: [ :each | PackageTreePackageGroupNodeModel with: each model: self ])
		sorted: [ :a :b | a name < b name ]