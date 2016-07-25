accessing array
handle: aHandle at: index put: value
	self isPointer ifTrue: [ ^ aHandle pointerAt: index put: value ].
	^ self basicHandle: aHandle at: index put: value