tests - testing
testIsQuick
	| method  |

	method := self class compiledMethodAt: #returnTrue.
	self assert: (method isQuick).

	method := self class compiledMethodAt: #returnPlusOne:.
	self deny: (method isQuick).

	