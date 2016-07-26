mc related utils
getSliceIssuesNumbers
	"Figure out which slices were loaded by looking at MC packages"
	
	^ MCWorkingCopy allManagers 
		select: [ :each | each packageName beginsWith: 'SLICE-Issue-' ]
		thenCollect: [ :each | ((each packageName allButFirst: 12) copyUpTo: $-) asNumber ]