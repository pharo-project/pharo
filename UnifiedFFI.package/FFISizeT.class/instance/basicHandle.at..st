private
basicHandle: aHandle at: index
	self flag: #todo. "x64"
	^ aHandle unsignedLongAt: index