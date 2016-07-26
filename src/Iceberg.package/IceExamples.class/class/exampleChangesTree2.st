examples - change tree
exampleChangesTree2
	| repository changeSet ds |
	repository := IceRepository origin: 'git@github.com:npasserini/pharo-git-test.git'.
	changeSet := IceDiff forRepository: repository.
	ds := FTTreeDataSource
		roots:	 (changeSet elements sort: [ :a :b | a name < b name ])
		children: [ :data | data elements sort: [ :a :b | a name < b name ] ].
	FTTableMorph new
		extent: 200 @ 400;
		dataSource: ds;
		openInWindow
