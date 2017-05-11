private
basicHandle: aHandle at: index put: value
	^ LibC memCopy: value getHandle to: aHandle size: self externalTypeSize
