accessing
removeWeakDependent: anObject
	FinalizationLock critical:[
		1 to: FinalizationDependents size do:[:i|
			((FinalizationDependents at: i) == anObject) ifTrue:[
				FinalizationDependents at: i put: nil.
			].
		].
	] ifError:[:msg :rcvr| rcvr error: msg].