printing
remove: oldObject ifAbsent: exceptionBlock
	"Remove oldObject as one of the receiver's elements."
	
	oldObject ifNil: [ ^nil ].
	^(self protected: [ valueDictionary removeKey: oldObject ifAbsent: nil ])
		ifNil: [ exceptionBlock value ]