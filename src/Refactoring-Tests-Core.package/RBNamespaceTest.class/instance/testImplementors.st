class tests
testImplementors
	| st |
	st := RBNamespace new.
	self assert: ((st allImplementorsOf: #printString) 
				includes: (st classNamed: #Object)).
	(st classNamed: #Object) removeMethod: #printString.
	self deny: ((st allImplementorsOf: #printString) 
				includes: (st classNamed: #Object))