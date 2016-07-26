accessing
doItReceiver
	^selectedContext ifNil: [ selectedProcess ] ifNotNil: [ selectedContext receiver ]