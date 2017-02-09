tests - object
testMutateFirstInstVarOfObjectWithManyVars
	| guineaPig failure |
	guineaPig := WriteBarrierStub new.
	guineaPig beReadOnlyObject.
	failure := [ guineaPig var1: #test ] on: ModificationForbidden do: [:err | err].

	self assert: failure fieldIndex equals: 1