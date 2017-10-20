tests - repositories
testHttpRepository
	gofer url: 'http://source.lukas-renggli.ch/pier' username: 'foo' password: 'bar'.
	self assert: gofer repositories: #('http://source.lukas-renggli.ch/pier').
	self assert: (gofer repositories first isKindOf: MCHttpRepository)