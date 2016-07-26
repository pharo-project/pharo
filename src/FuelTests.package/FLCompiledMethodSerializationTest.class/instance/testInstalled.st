tests
testInstalled
	| materialized |
	"Since Pharo 3.0 decompilation of compiled methods is no longer possible.
	That means we have to store the source too."
	self useInstalled.
	
	self assert: theCompiledMethod isInstalled.
	self deny: theCompiledMethod isDoIt.
	self deny: theCompiledMethod trailer hasSource.
	self deny: theCompiledMethod trailer isEmpty.
	self assert: theCompiledMethod trailer hasSourcePointer.
	
	"if installed but not different, the installed instance will be answered"
	materialized := self resultOfSerializeAndMaterialize: theCompiledMethod.
	self assert: materialized == theCompiledMethod