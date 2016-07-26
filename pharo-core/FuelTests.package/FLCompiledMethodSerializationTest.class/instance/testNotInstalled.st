tests
testNotInstalled
	| materialized |
	"Since Pharo 3.0 decompilation of compiled methods is no longer possible.
	That means we have to store the source too."
	self useNotInstalled.
	
	self deny: theCompiledMethod isInstalled.
	self deny: theCompiledMethod isDoIt.
	self deny: theCompiledMethod trailer hasSource.
	self assert: theCompiledMethod trailer isEmpty.
	self deny: theCompiledMethod trailer hasSourcePointer.
	
	materialized := self resultOfSerializeAndMaterialize: theCompiledMethod.
	self deny: materialized isInstalled.
	self deny: materialized isDoIt.
	"we serialized the source"
	self assert: materialized trailer hasSource.
	self deny: materialized trailer isEmpty.
	self deny: materialized trailer hasSourcePointer.
	
	self assert: (materialized isEqualRegardlessTrailerTo: theCompiledMethod)