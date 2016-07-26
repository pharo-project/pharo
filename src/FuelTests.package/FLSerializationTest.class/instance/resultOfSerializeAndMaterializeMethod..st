running
resultOfSerializeAndMaterializeMethod: aCompiledMethod
	self analyzer considerInternalMethod: aCompiledMethod. 
	self serialize: aCompiledMethod.
	^ self materialized
