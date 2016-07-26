private-test results
displayTestResultKind: kind with: aPrefix

	^ self 
		displayString: (' [', aPrefix, kind, ']') 
		color: (self colorByKindOfTestResult at: kind)