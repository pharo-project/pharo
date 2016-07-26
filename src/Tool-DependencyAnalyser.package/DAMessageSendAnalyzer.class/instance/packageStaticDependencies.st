accessing
packageStaticDependencies	
	^ packageAnalysis dependencies 
		collect: [:each | each target packageName ]
		as: Set