tests-clean
testNestedBlockClosure
	| closure materializedClosure |
	closure := [ [ 42 ] ].
	materializedClosure := self resultOfSerializeAndMaterialize: closure.
	closure assertWellMaterializedInto: materializedClosure in: self.
	self assert: materializedClosure value value = 42