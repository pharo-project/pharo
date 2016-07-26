tests
testDifference
    | project expected fromVersionString toVersionString report actual |
    project := self project.
    fromVersionString := '1.1-baseline'.
    toVersionString := '2.0-baseline'.
    report := (project version: fromVersionString) difference: (project version: toVersionString).
    report
        from: fromVersionString;
        to: toVersionString;
        configuration: 'Test Configuration'.
    expected := 'Test Configuration ''1.1-baseline'' to ''2.0-baseline''
	Additions:
		UTF9
			'''' to ''1.0-baseline''
	Modifications:
		UTF8
			''1.0-baseline'' to ''stable''
	Removals:
		UTF7
			''1.0-baseline'' to ''''
'.
    actual := report printString.
    self assert: expected = actual