accessing
fingersFromDevice: id
	^ devicesFingers at: id ifAbsent: [ ^ fingers values ]