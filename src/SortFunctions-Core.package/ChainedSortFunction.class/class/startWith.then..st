instance creation
startWith: aSortFunction then: aSortFunction2
	^self new
		startFunction: aSortFunction;
		nextFunction: aSortFunction2