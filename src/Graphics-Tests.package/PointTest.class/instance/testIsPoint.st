tests - converting
testIsPoint

	self deny: nil isPoint.
	self deny: 'a' isPoint.
	self deny: 1 isPoint. 
	self deny: -1 isPoint. 
	'Test on a random class'.
	self deny: KernelClassesFilter new isPoint.
	self deny: Object new isPoint.
	self assert: Point new isPoint.
	self assert: (0 @ 0) isPoint.
	self assert: (-1 @ -1) isPoint.
	self assert: (1.2 @ 1.2) isPoint.
	self assert: (-1.2 @ -1.2) isPoint.