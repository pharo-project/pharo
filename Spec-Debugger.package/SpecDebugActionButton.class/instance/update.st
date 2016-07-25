updating
update

	self debugAction ifNotNil:  [ :aDebugAction | 
		self label: aDebugAction label.
		self icon: aDebugAction icon.
		self enabled: aDebugAction enabled.
		self state: false.
		self action: [ aDebugAction execute ] ]