accessing array
handle: aHandle at: index 
	self isPointer ifTrue: [ ^ aHandle pointerAt: index ].
	^ self basicHandle: aHandle at: index