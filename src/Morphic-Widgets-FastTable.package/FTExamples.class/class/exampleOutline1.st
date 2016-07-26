examples
exampleOutline1
	<example>
	| list |
	list := FTTableMorph new
		extent: 200 @ 400;
		dataSource:
			(FTOutlineDataSource
				items:
					{(1 to: 10).
					(11 to: 20).
					(21 to: 30).
					(31 to: 40).
					(41 to: 50)}).
	list openInWindow