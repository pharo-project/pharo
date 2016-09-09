class tests
testIncludesClass
	self assert: (RBClassModelFactory rbNamespace new includesClassNamed: #Object).
	self deny: (RBClassModelFactory rbNamespace new includesClassNamed: #Object1).
	self 
		deny: ((RBClassModelFactory rbNamespace 
				onEnvironment: (RBClassEnvironment onEnvironment: RBBrowserEnvironment new
						classes: (Array with: Object))) 
					includesClassNamed: #OrderedCollection)