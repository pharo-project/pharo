image package loading
allManagers

	^ Smalltalk globals at: #MCWorkingCopy 
		ifPresent: [:mcwc | mcwc allManagers ] 
		ifAbsent: [OrderedCollection new ]
