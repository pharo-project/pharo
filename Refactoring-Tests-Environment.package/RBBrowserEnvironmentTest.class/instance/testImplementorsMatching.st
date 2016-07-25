testing
testImplementorsMatching
	| print |
	print := RBBrowserEnvironment new implementorsMatching: '*print*'.
	self universalTestFor: print.
	self assert: (print implementorsOf: #printString) numberSelectors 
				= (RBBrowserEnvironment new implementorsOf: #printString) numberSelectors.
	print 
		classesAndSelectorsDo: [:class :sel | self assert: ('*print*' match: sel)]