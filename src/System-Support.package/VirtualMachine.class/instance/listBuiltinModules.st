modules
listBuiltinModules
	"Smalltalk vm listBuiltinModules"

	"Return a list of all builtin modules (e.g., plugins). Builtin plugins are those that are 
	compiled with the VM directly, as opposed to plugins residing in an external shared library. 	
	The list will include all builtin plugins regardless of whether they are currently loaded 
	or not. Note that the list returned is not sorted!"

	| modules index tname |
	modules := Array new writeStream.
	index := 1.
	[ true ]
		whileTrue: [ 
			tname := self listBuiltinModule: index.
			tname ifNil: [ ^ modules contents ].
			modules nextPut: tname.
			index := index + 1 ]