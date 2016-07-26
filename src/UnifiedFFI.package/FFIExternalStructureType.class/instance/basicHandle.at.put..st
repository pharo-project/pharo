private
basicHandle: aHandle at: index put: value
	^ self notYetImplemented.
	"Not sure if this is really OK:
	^ LibC memCopy: value getHandle to: aHandle size: self externalTypeSize"
