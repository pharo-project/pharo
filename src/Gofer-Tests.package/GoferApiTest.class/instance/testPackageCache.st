tests - repositories - options
testPackageCache
	gofer squeaksource: 'r1'; squeaksource: 'r2'.
	
	gofer enablePackageCache.
	self assert: gofer repositories: (Array with: MCCacheRepository uniqueInstance description) , #('http://www.squeaksource.com/r1' 'http://www.squeaksource.com/r2').
	
	gofer disablePackageCache.
	self assert: gofer repositories: #('http://www.squeaksource.com/r1' 'http://www.squeaksource.com/r2')