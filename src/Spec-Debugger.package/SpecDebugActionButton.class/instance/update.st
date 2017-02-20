updating
update

	self debugAction ifNotNil:  [ :aDebugAction | 
		self label: aDebugAction label.
		self help: aDebugAction help, Character tab asString, (aDebugAction keymap ifNil: [ '' ] ifNotNil: [ :k | k   printString]).
		self icon: aDebugAction icon.
		self enabled: aDebugAction enabled.
		self state: false.
		self action: [ aDebugAction execute ] ]