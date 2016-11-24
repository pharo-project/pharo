testing
addNativeBoostComponents
	Smalltalk
		at: #NBExternalHandle
		ifPresent: [ :handle | ^ self ].
		
	Object variableByteSubclass: #NBExternalHandle
		instanceVariableNames: ''
		classVariableNames: ''
		category: self hacksCategoryName