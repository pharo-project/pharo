running
resultOfSerializeAndMaterializeCompiledMethod: aCompiledMethod
	| materialized |
	materialized := self resultOfSerializeAndMaterialize: aCompiledMethod.
	self assert: (materialized isEqualRegardlessTrailerTo: aCompiledMethod)