source code area
doItReceiver

	^ self selectedClass 
		ifNotNil: [:class | class theNonMetaClass ]