running
resultOfSerializeAndMaterializeMethodDictionary: aMethodDictionary
	| materialized |
	materialized := self resultOfSerializeAndMaterialize: aMethodDictionary.
	self assert: (materialized isEqualRegardlessMethodsTrailerTo: aMethodDictionary)