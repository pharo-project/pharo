special context access
unwindComplete: aBoolean
	"unwindContext only. access temporaries from BlockClosure>>#ensure: and BlockClosure>>#ifCurtailed:"
	self tempAt: 2 put: aBoolean
