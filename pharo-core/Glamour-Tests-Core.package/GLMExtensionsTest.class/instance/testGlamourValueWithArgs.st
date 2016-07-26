tests
testGlamourValueWithArgs
	"Block closure"

	self assert: ([ #foo ] glamourValueWithArgs: #(1 2)) equals: #foo.
	self assert: ([ :first | Array with: first ] glamourValueWithArgs: #(1 2)) equals: #(1).
	self assert: ([ :first :second | Array with: first with: second ] glamourValueWithArgs: #(1 2)) equals: #(1 2).
	self
		assert: ([ :first :second :third | Array with: first with: second with: third ] glamourValueWithArgs: #(1 2))
		equals: #(1 2 nil).	"Symbol"
	self assert: (#yourself glamourValueWithArgs: #(1 2)) equals: 1.
	self assert: (#yourself glamourValueWithArgs: #()) equals: nil