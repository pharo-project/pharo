as yet unclassified
testInstalledModified
	| copy materialized |
	"Since Pharo 3.0 decompilation of compiled methods is no longer possible.
	That means we have to store the source too."
	self useInstalled.
	
	self assert: theCompiledMethod isInstalled.
	self deny: theCompiledMethod isDoIt.
	self deny: theCompiledMethod trailer hasSource.
	self deny: theCompiledMethod trailer isEmpty.
	self assert: theCompiledMethod trailer hasSourcePointer.
	
	copy := theCompiledMethod copy.
	"different instance can not be installed at the same time."
	self deny: copy isInstalled.
	self deny: copy isDoIt.
	self deny: copy trailer hasSource.
	self deny: copy trailer isEmpty.
	self assert: copy trailer hasSourcePointer.
	
	"if installed but not different, the installed instance will be answered"
	materialized := self resultOfSerializeAndMaterialize: copy.
	self deny: materialized == theCompiledMethod.
	self deny: materialized == copy.
	self deny: materialized isInstalled.
	self deny: materialized isDoIt.
	"we serialized the source"
	self assert: materialized trailer hasSource.
	self deny: materialized trailer isEmpty.
	self deny: materialized trailer hasSourcePointer