initialize-release
updateInstancesFrom: oldClass
	"Recreate any existing instances of the argument, oldClass, as instances of 
	the receiver, which is a newly changed class. Permute variables as 
	necessary. Return the array of old instances (none of which should be
	pointed to legally by anyone but the array)."
	"ar 7/15/1999: The updating below is possibly dangerous. If there are any
	contexts having an old instance as receiver it might crash the system if
	the new receiver in which the context is executed has a different layout.
	See bottom below for a simple example:"
	| oldInstances |
	oldInstances := oldClass allInstances asArray.
	oldInstances := self updateInstances: oldInstances from: oldClass isMeta: self isMeta.
	^nil

"	| crashingBlock class |
	class := Object subclass: #CrashTestDummy
		instanceVariableNames: 'instVar'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Crash-Test'.
	class compile:'instVar: value instVar := value'.
	class compile:'crashingBlock ^[instVar]'.
	crashingBlock := (class new) instVar: 42; crashingBlock.
	Object subclass: #CrashTestDummy
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Crash-Test'.
	crashingBlock.
	crashingBlock value.
	"
