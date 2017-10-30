tests
testVersionShouldParseComplexName
	| queryReference |
	queryReference := GoferVersionReference name: 'Seaside2.8b5'.
	self assert: queryReference packageName = 'Seaside2'.
	self assert: queryReference author isEmpty.
	self assert: queryReference branch = '8b5'.
	self assert: queryReference versionNumber = 0.
	
	queryReference := GoferVersionReference name: 'Seaside2.8b5-avi.1'.
	self assert: queryReference packageName = 'Seaside2'.
	self assert: queryReference author = 'avi'.
	self assert: queryReference branch = '8b5'.
	self assert: queryReference versionNumber = 1.
	
	queryReference := GoferVersionReference name: 'Seaside-Core-pmm.2'.
	self assert: queryReference packageName = 'Seaside-Core'.
	self assert: queryReference author = 'pmm'.
	self assert: queryReference branch isEmpty.
	self assert: queryReference versionNumber = 2.
	
	queryReference := GoferVersionReference name: 'Seaside-Core.configcleanup-jf.3'.
	self assert: queryReference packageName = 'Seaside-Core'.
	self assert: queryReference author = 'jf'.
	self assert: queryReference branch = 'configcleanup'.
	self assert: queryReference versionNumber = 3.
	
	queryReference := GoferVersionReference name: 'Seaside-Core.configcleanup.extraspeedup-lr.69'.
	self assert: queryReference packageName = 'Seaside-Core'.
	self assert: queryReference author = 'lr'.
	self assert: queryReference branch = 'configcleanup.extraspeedup'.
	self assert: queryReference versionNumber = 69.

	queryReference := GoferVersionReference name: 'Seaside-Core.configcleanup42.extraspeedup-lr.69'.
	self assert: queryReference packageName = 'Seaside-Core'.
	self assert: queryReference author = 'lr'.
	self assert: queryReference branch = 'configcleanup42.extraspeedup'.
	self assert: queryReference versionNumber = 69
