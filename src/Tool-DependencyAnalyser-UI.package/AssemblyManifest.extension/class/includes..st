*Tool-DependencyAnalyser-UI
includes: aPackageName
	^ (self parts collect: #key) 
		includes: aPackageName