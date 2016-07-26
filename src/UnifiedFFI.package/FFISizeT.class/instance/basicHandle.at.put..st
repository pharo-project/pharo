private
basicHandle: aHandle at: index put: value
	self flag: #todo. "x64"
	^ aHandle unsignedLongAt: index put: value