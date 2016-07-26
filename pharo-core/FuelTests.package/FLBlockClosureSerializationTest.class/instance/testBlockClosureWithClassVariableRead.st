tests-clean
testBlockClosureWithClassVariableRead
	| closure materializedClosure |
	ClassVariableForTesting := nil.
	closure := [ ClassVariableForTesting ].
	materializedClosure := self resultOfSerializeAndMaterialize: closure.
	closure assertWellMaterializedInto: materializedClosure in: self.

	ClassVariableForTesting := true.
	self assert: materializedClosure value.
	ClassVariableForTesting := false.
	self deny: materializedClosure value