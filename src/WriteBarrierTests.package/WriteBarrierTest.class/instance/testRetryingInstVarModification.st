tests - object
testRetryingInstVarModification
	| guineaPig |
	guineaPig := MessageSend new.
	guineaPig beReadOnlyObject.

	[ guineaPig receiver: 1 ] on: ModificationForbidden do: [:err | 
		guineaPig beWritableObject.
		err retryModification ].

	self assert: guineaPig receiver equals: 1