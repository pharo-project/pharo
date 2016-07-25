modules
listLoadedModules
	"SmalltalkImage current listLoadedModules"
	"Return a list of all currently loaded modules (e.g., plugins). Loaded modules are those that currently in use (e.g., active). The list returned will contain all currently active modules regardless of whether they're builtin (that is compiled with the VM) or external (e.g., residing in some external shared library). Note that the returned list is not sorted!"
	| modules index tname |
	modules := Array new writeStream.
	index := 1.
	[true] whileTrue: [
		tname := self listLoadedModule: index.
		tname ifNil:[^modules contents].
		modules nextPut: tname.
		index := index + 1.
	].