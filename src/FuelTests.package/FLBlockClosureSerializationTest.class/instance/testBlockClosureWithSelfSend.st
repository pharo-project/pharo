tests-unclean
testBlockClosureWithSelfSend
	| closure materializedClosure |
	closure := self class blockClosureWithSelfSend.
	materializedClosure := self resultOfSerializeAndMaterialize: closure.
	closure assertWellMaterializedInto: materializedClosure in: self.
	self assert: materializedClosure value = closure value