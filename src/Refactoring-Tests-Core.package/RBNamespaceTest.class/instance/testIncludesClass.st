class tests
testIncludesClass
	self assert: (RBNamespace new includesClassNamed: #Object).
	self deny: (RBNamespace new includesClassNamed: #Object1).
	self 
		deny: ((RBNamespace 
				onEnvironment: (RBClassEnvironment onEnvironment: RBBrowserEnvironment new
						classes: (Array with: Object))) 
					includesClassNamed: #OrderedCollection)