ui
clean
	self allInstances do:[ :a| a unregister].
	Smalltalk garbageCollect.