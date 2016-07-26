special context access
unwindBlock
	"unwindContext only. access temporaries from BlockClosure>>#ensure: and BlockClosure>>#ifCurtailed:"
	^self tempAt: 1
