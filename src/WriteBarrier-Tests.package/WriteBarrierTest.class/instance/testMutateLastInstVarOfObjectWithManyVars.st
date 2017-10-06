tests - object
testMutateLastInstVarOfObjectWithManyVars
	| guineaPig failure |
	guineaPig := WriteBarrierStub new.
	guineaPig beReadOnlyObject.
	failure := [ guineaPig var10: #test ] on: ModificationForbidden do: [:err | err].

	self assert: failure fieldIndex equals: 10