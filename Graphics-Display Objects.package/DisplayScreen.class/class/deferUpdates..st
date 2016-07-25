deferring
deferUpdates: aBoolean
	| wasDeferred |
	"Set the deferUpdates flag in the virtual machine. When this flag is true, BitBlt operations on the Display are not automatically propagated to the screen. If this underlying platform does not support deferred updates, this primitive will fail. Answer whether updates were deferred before if the primitive succeeds, nil if it fails."

	wasDeferred := DeferringUpdates == true.
	DeferringUpdates := aBoolean.
	^(self primitiveDeferUpdates: aBoolean) ifNotNil: [wasDeferred]