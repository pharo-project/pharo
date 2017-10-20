tests - object
testMutateInstVarShouldCatchRightFailure
	| guineaPig failure |
	guineaPig := MessageSend new.
	guineaPig beReadOnlyObject.
	failure := [ guineaPig receiver: #test ] on: ModificationForbidden do: [:err | err].

	self assert: failure object == guineaPig.
	self assert: failure newValue equals: #test.
	self assert: failure fieldIndex equals: 1.