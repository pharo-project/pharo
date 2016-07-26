tests-clean
testBlockClosureWithTempVariableRead
	| closure materializedClosure |
	closure := self class blockClosureWithTempVariableRead.
	materializedClosure := self resultOfSerializeAndMaterialize: closure.
	closure assertWellMaterializedInto: materializedClosure in: self.
	self assert: materializedClosure value = 'TEST'.
